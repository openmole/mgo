/*
 * Copyright (C) 2019 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mgo.test

import mgo.abc._
import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution
import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils }
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.util.Random

object IdentityMonAPMC extends App {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global
  implicit val rng: Random = new util.Random(42)

  // A deterministic model.
  def toyModel(theta: Vector[Double], rng: util.Random): Vector[Double] =
    theta.map { _ + rng.nextDouble / Double.PositiveInfinity }

  // Test MonAPMC and its Exposed interface
  val p: MonAPMC.Params = MonAPMC.Params(
    apmcP = APMC.Params(
      n = 20,
      nAlpha = 10,
      pAccMin = 0.01,
      priorSample = rng => Array(rng.nextDouble() * 20 - 10),
      priorDensity = {
        case Array(x) =>
          if (x >= -10 && x <= 10) { 1.0 / 20.0 }
          else 0
      },
      observed = Array(1.3)),
    stopSampleSizeFactor = 5)

  var exceptionCaught: Option[APMC.SingularCovarianceException] = None
  try {
    MonAPMC.scan(p, toyModel, 1, 1)
  } catch {
    case e: APMC.SingularCovarianceException =>
      exceptionCaught = Some(e)
  }

  exceptionCaught match {
    case Some(e) =>
      println("Test successful: SingularCovarianceException thrown as expected with a deterministic model.")
      println(e)
    case None => println("Test failed: SingularCovarianceException expected.")
  }
}

object GaussianMix2DMonAPMC extends App {

  implicit val rng: Random = new util.Random(42)
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  // Gaussian Mixture 1D toy model
  def toyModel(theta: Vector[Double], rng: util.Random): Vector[Double] = {
    val cov1: Array[Array[Double]] = Array(
      Array(1.0 / 2.0, -0.4),
      Array(-0.4, 1.0 / 2.0))
    val cov2: Array[Array[Double]] = Array(
      Array(1 / 100.0, 0.0),
      Array(0.0, 1 / 100.0))
    assert(new LUDecomposition(MatrixUtils.createRealMatrix(cov1)).getDeterminant() != 0)
    assert(new LUDecomposition(MatrixUtils.createRealMatrix(cov2)).getDeterminant() != 0)
    val mixtureWeights = Array(0.5, 0.5)
    val translate = 1
    val mean1 = theta.map { _ - translate }.toArray
    val mean2 = theta.map { _ + translate }.toArray
    val dist = new MixtureMultivariateNormalDistribution(
      mixtureWeights, Array(mean1, mean2), Array(cov1, cov2))
    dist.sample.toVector
  }

  val p: MonAPMC.Params = MonAPMC.Params(
    apmcP = APMC.Params(
      n = 5000,
      nAlpha = 500,
      pAccMin = 0.01,
      priorSample = rng => Array(
        rng.nextDouble() * 20 - 10,
        rng.nextDouble() * 20 - 10),
      priorDensity = {
        case Array(x, y) =>
          if (x >= -10 && x <= 10 &&
            y >= -10 && y <= 10) {
            1.0 / (20.0 * 20.0)
          } else 0
      },
      observed = Array(0, 0)),
    stopSampleSizeFactor = 5)

  def histogram(xs: Vector[(Double, Double)], ws: Vector[Double],
    lowerBound: (Double, Double),
    upperBound: (Double, Double), bins: (Int, Int)): (Vector[Double], Vector[Double], Vector[Vector[Double]]) = {
    val width: (Double, Double) = ((upperBound._1 - lowerBound._1) / bins._1.toDouble,
      (upperBound._2 - lowerBound._2) / bins._2.toDouble)
    val total = ws.sum
    def toBin(x: (Double, Double)): (Double, Double) =
      (width._1 * (x._1 / width._1).floor, width._2 * (x._2 / width._2).floor)
    val histMap: Map[(Double, Double), Double] = (xs zip ws)
      .groupBy { case (x, w) => toBin(x) }
      .mapValues { xws =>
        val wsum = xws.map { case (_, w) => w }.sum
        wsum / (width._1 * width._2 * total).toDouble
      }.toMap

    val xBins = (BigDecimal(lowerBound._1) to upperBound._1 by width._1).toVector.map(_.toDouble)
    val yBins = (BigDecimal(lowerBound._2) to upperBound._2 by width._2).toVector.map(_.toDouble)

    val z = yBins.map { by =>
      xBins.map { bx =>
        (histMap.getOrElse(toBin((bx, by)), 0.0))
      }
    }

    (xBins, yBins, z)
  }

  def report(ss: Vector[APMC.State]): Unit = {
    println("epsilon\tpAcc\tsample size")
    println(ss.map { s =>
      s.epsilon.toString ++ "\t" ++
        s.pAcc.toString ++ "\t" ++
        s.weights.size.toString
    }.mkString("\n"))
    println("\n")
    println("\n")
    println("w q0.25\tw q0.5\tw q0.75")
    println(ss.map { s =>
      val wstat = new DescriptiveStatistics(s.weights)

      "%.3f\t%.3f\t%.3f".format(
        wstat.getPercentile(25.0),
        wstat.getPercentile(50.0),
        wstat.getPercentile(75.0))
    }.mkString("\n"))
    println("\n")

    reportS(ss.last)
  }

  def reportS(s: APMC.State): Unit = {
    println("Epsilon = " ++ s.epsilon.toString)
    println()

    val thetasArray = s.thetas.map { case Array(x, y) => (x, y) }

    println("\nThetas histogram:")

    val (xBins, yBins, z) = histogram(thetasArray.toVector, s.weights.toVector, (-3, -3), (4, 4), (30, 30))

    val zstat = new DescriptiveStatistics(z.flatten.toArray.filter { _ > 0 })
    val zquarts = (
      zstat.getPercentile(25.0),
      zstat.getPercentile(50.0),
      zstat.getPercentile(75.0))
    val zmin = zstat.getMin()
    val zmax = zstat.getMax()
    println(zquarts)

    println("Z min quartiles max: %.4f %.4f %.4f %.4f %.4f".format(
      zmin, zquarts._1, zquarts._2, zquarts._3, zmax))

    val maxWidth = 80
    val maxHeight = 80
    (xBins zip z).reverse.foreach {
      case (xb, zrow) =>
        println("%- 3.2f| ".format(xb) ++
          zrow.map { z =>
            if (z < zquarts._1) "⬝"
            else if (z < zquarts._2) "○"
            else if (z < zquarts._3) "◉"
            else "●"
          }.mkString(" "))
    }
  }
  // "⬝ ⚬ ○ ◉ ●
  println("---- 2D Gaussian Mixture; APMC ----")
  report(APMC.scan(p.apmcP, toyModel))

  println("---- 2D Gaussian Mixture; MonAPMC ----")
  report(MonAPMC.scan(p, toyModel, 1, 1).collect { case MonAPMC.State(_, s) => s })

  println("---- 2D Gaussian Mixture; MonAPMC parallel 2----")
  report(MonAPMC.scan(p, toyModel, 1, 2).collect { case MonAPMC.State(_, s) => s })

  println("---- 2D Gaussian Mixture; MonAPMC parallel 1, stepSize 2----")
  report(MonAPMC.scan(p, toyModel, 2, 1).collect { case MonAPMC.State(_, s) => s })
}

