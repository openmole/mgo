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
import mgo.evolution._
import mgo.evolution.contexts._
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.Well1024a
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import scala.concurrent.ExecutionContext
import scala.math._
import scala.util.{ Try, Failure, Success }

object GaussianMix1DMonAPMC extends App {
  implicit val ec = ExecutionContext.global
  implicit val rng = new Well1024a()

  // Gaussian Mixture 1D toy model
  object ToyModel {
    val var1 = 1.0 / 100.0
    val var2 = 1.0

    def toyModel(theta: Vector[Double])(implicit rng: RandomGenerator): Vector[Double] = Vector(
      if (rng.nextBoolean) { theta.head + rng.nextGaussian * sqrt(var1) }
      else { theta.head + rng.nextGaussian * sqrt(var2) })

    // P[Theta = theta | x]
    def posterior(x: Double, theta: Double): Double =
      0.5 * new NormalDistribution(rng, theta, sqrt(var1))
        .density(x) +
        0.5 * new NormalDistribution(rng, theta, sqrt(var2)).density(x)

    // P[Theta < theta | x]
    def posteriorCDF(x: Double, theta: Double): Double =
      0.5 * new NormalDistribution(rng, theta, sqrt(var1))
        .cumulativeProbability(x) +
        0.5 * new NormalDistribution(rng, theta, sqrt(var2)).cumulativeProbability(x)
  }

  // Test MonAPMC and its Exposed interface
  val p = APMC.Params(
    n = 5000,
    nAlpha = 500,
    pAccMin = 0.01,
    priorSample = () => Array(rng.nextDouble() * 20 - 10),
    priorDensity = {
      case Array(x) =>
        if (x >= -10 && x <= 10) { 1.0 / 20.0 }
        else 0
    },
    observed = Array(0))

  // Compute L2 between posterior sample and theoretical cumulative distribution function.
  def posteriorL2(lowerBound: Double, upperBound: Double, bins: Int,
    weightsXs: Vector[(Double, Double)]): Double = {
    val binWidth = (upperBound - lowerBound) / bins.toDouble
    def theoPostBin(bin: Double): Double =
      (ToyModel.posteriorCDF(bin + binWidth, p.observed.head) -
        ToyModel.posteriorCDF(bin, p.observed.head))
    def toBin(x: Double): Double =
      lowerBound + binWidth * floor((x - lowerBound) / binWidth)
    val sumWeights = weightsXs.map { _._1 }.sum
    val estPostBin = weightsXs
      .groupBy { case (w, x) => toBin(x) }
      .mapValues { _.map { _._1 }.sum / sumWeights }

    sqrt(estPostBin.map { case (b, e) => pow(e - theoPostBin(b), 2) }.sum)
  }

  def histogram(xs: Vector[Double], ws: Vector[Double], lowerBound: Double, upperBound: Double, bins: Int): Vector[(Double, Double)] = {
    val width = (upperBound - lowerBound) / bins.toDouble
    val total = ws.sum
    def toBin(x: Double): Double =
      width * (x / width).floor
    val histMap: Map[Double, Double] = (xs zip ws)
      .groupBy { case (x, w) => toBin(x + width / 2.0) }
      .mapValues { xws =>
        val wsum = xws.map { case (x, w) => w }.sum
        wsum / (width * total).toDouble
      }

    (lowerBound to upperBound by width).toVector
      .map { b => (b, histMap.getOrElse(toBin(b), 0.0)) }
  }

  def report(ss: Vector[APMC.State]): Unit = {
    println("epsilon\tpAcc\tl2")
    println(ss.map { s =>
      val l2 = posteriorL2(-10, 10, 300,
        (s.weights zip s.thetas.getData.map { _.head }).toVector)
      "%f\t%f\t%f".format(s.epsilon, s.pAcc, l2)
    }.mkString("\n"))

    reportS(ss.last)
  }

  def reportS(s: APMC.State): Unit = {
    println("Epsilon = " ++ s.epsilon.toString)

    val thetasArray = s.thetas.getData.map { _.head }
    val statsTheta = new DescriptiveStatistics(thetasArray)
    val l2 = posteriorL2(-10, 10, 300,
      (s.weights zip s.thetas.getData.map { _.head }).toVector)
    println("Theta Mean = " ++ statsTheta.getMean.toString)
    println("Theta Standard Deviation = " ++
      statsTheta.getStandardDeviation.toString)
    println("L2 = " ++ l2.toString)

    println("\nThetas histogram:")
    val h = histogram(thetasArray.toVector, s.weights.toVector, -2.5, 2.5, 20)
    val maxWidth = 30
    println("  bin  theo  est")
    h.foreach {
      case (bin, height) =>
        val theo = ToyModel.posterior(bin, p.observed.head)
        println(f"$bin% 3.2f: $theo%.3f $height%.3f " ++
          Iterator.fill((height * maxWidth).round.toInt)("●").mkString(""))
    }
  }

  println("---- 1D Gaussian Mixture; APMC ----")
  report(APMC.scan(p, ToyModel.toyModel))

  println("---- 1D Gaussian Mixture; MonAPMC stepSize 1 parallel 1 ----")
  report(
    MonAPMC.scan(p, ToyModel.toyModel, 1, 1)
      .collect { case MonAPMC.State(_, s) => s })

  println("---- 1D Gaussian Mixture; MonAPMC stepSize 1 parallel 2 ----")
  report(MonAPMC.scan(p, ToyModel.toyModel, 1, 2)
    .collect { case MonAPMC.State(_, s) => s })

  println("---- 1D Gaussian Mixture; MonAPMC stepSize 2 parallel 1 ----")
  report(
    MonAPMC.scan(p, ToyModel.toyModel, 2, 1)
      .collect { case MonAPMC.State(_, s) => s })
}

object GaussianMix2DMonAPMC extends App {

  implicit val rng = new Well1024a()
  implicit val ec = ExecutionContext.global

  // Gaussian Mixture 1D toy model
  def toyModel(theta: Vector[Double])(implicit rng: RandomGenerator): Vector[Double] = {
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

  val p = APMC.Params(
    n = 5000,
    nAlpha = 500,
    pAccMin = 0.01,
    priorSample = () => Array(
      rng.nextDouble() * 20 - 10,
      rng.nextDouble() * 20 - 10),
    priorDensity = {
      case Array(x, y) =>
        if (x >= -10 && x <= 10 &&
          y >= -10 && y <= 10) {
          1.0 / (20.0 * 20.0)
        } else 0
    },
    observed = Array(0, 0))

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
      }

    val xBins = (lowerBound._1 to upperBound._1 by width._1).toVector
    val yBins = (lowerBound._2 to upperBound._2 by width._2).toVector

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

    val thetasArray = s.thetas.getData.map { case Array(x, y) => (x, y) }

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
  report(APMC.scan(p, toyModel))

  println("---- 2D Gaussian Mixture; MonAPMC ----")
  report(MonAPMC.scan(p, toyModel, 1, 1).collect { case MonAPMC.State(_, s) => s })

  println("---- 2D Gaussian Mixture; MonAPMC parallel 2----")
  report(MonAPMC.scan(p, toyModel, 1, 2).collect { case MonAPMC.State(_, s) => s })

  println("---- 2D Gaussian Mixture; MonAPMC parallel 1, stepSize 2----")
  report(MonAPMC.scan(p, toyModel, 2, 1).collect { case MonAPMC.State(_, s) => s })
}

