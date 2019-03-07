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
import freedsl.dsl._
import mgo.evolution._
import mgo.evolution.contexts._
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.Well1024a
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import java.nio.file.Files
import java.nio.file.Paths

object GaussianMixMonAPMC extends App {

  implicit val rng = new Well1024a()

  // Gaussian Mixture 1D toy model
  def toyModel(theta: Vector[Double])(implicit rng: RandomGenerator): Vector[Double] = Vector(
    if (rng.nextBoolean) { theta.head + rng.nextGaussian * (1.0 / 10.0) }
    else { theta.head + rng.nextGaussian })

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

  def histogram(xs: Vector[Double], ws: Vector[Double], lowerBound: Double, upperBound: Double, bins: Int): Vector[(Double, Double)] = {
    val width = (upperBound - lowerBound) / bins.toDouble
    val total = ws.sum
    def toBin(x: Double): Double =
      width * (x / width).floor
    val histMap: Map[Double, Double] = (xs zip ws)
      .groupBy { case (x, w) => toBin(x) }
      .mapValues { xws =>
        val wsum = xws.map { case (x, w) => w }.sum
        wsum / (width * total).toDouble
      }

    (lowerBound to upperBound by width).toVector
      .map { b => (b, histMap.getOrElse(toBin(b), 0.0)) }
  }

  def report(ss: Vector[APMC.State]): Unit = {
    println(ss.map { s => (s.epsilon, s.pAcc) }.mkString("\n"))

    reportS(ss.last)
  }

  def reportS(s: APMC.State): Unit = {
    println("Epsilon = " ++ s.epsilon.toString)

    val thetasArray = s.thetas.getData.map { _.head }
    val statsTheta = new DescriptiveStatistics(thetasArray)
    println("Theta Mean = " ++ statsTheta.getMean.toString)
    println("Theta Standard Deviation = " ++
      statsTheta.getStandardDeviation.toString)

    println("\nThetas histogram:")
    val h = histogram(thetasArray.toVector, s.weights.toVector, -2.5, 2.5, 20)
    val maxWidth = 50
    h.foreach {
      case (bin, height) =>
        println(f"$bin% 3.2f: $height%.3f " ++
          Iterator.fill((height * 50).round.toInt)("*").mkString(""))
    }

    Files.write(
      Paths.get("/tmp/mgoTestMonAPMC.txt"),
      thetasArray.mkString("\n").getBytes())

  }

  println("---- MonAPMC.exposedEval ----")
  report(MonAPMC.exposedEval(p).scan(toyModel).map { case (MonAPMC.State(_, s)) => s })

  println("---- APMC.run ----")
  report(APMC.scan(p, toyModel))
}

