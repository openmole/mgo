/*
 * Copyright (C) Guillaume Chérel 10/07/2017
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

import math._

import mgo._

import algorithm.monteCarlo.RejectionSampling
import algorithm.monteCarlo.MCSampling.context.implicits._

object DiskRejectionSampling extends App {

  def pdfUniformCircle(x: (Double, Double)): Double =
    if (inCircle(x))
      1.0 / Pi
    else
      0.0

  def inSquare(x: (Double, Double)): Boolean =
    x._1 >= -1 && x._1 < 1 && x._2 >= -1 && x._2 < 1

  def inCircle(x: (Double, Double)): Boolean =
    sqrt(pow(x._1, 2) + pow(x._2, 2)) <= 1

  def vec2Tup(x: Vector[Double]): (Double, Double) = (x(0), x(1))
  def tup2Vec(x: (Double, Double)): Vector[Double] = Vector(x._1, x._2)
  def ifElse[A](yes: A, no: A)(x: Boolean): A = if (x) yes else no

  val mcsampling = RejectionSampling(
    qSample = { r => Vector(r.nextDouble * 2 - 1, r.nextDouble * 2 - 1) },
    qPdf = ifElse(0.25, 0.0) _ compose inSquare compose vec2Tup,
    m = 4.0 / Pi,
    pPdf = pdfUniformCircle _ compose vec2Tup
  )

  val (finalState, finalPopulation) =
    run(mcsampling).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  val finalSamples: Vector[Vector[Double]] = RejectionSampling.result(finalPopulation)

  println(finalPopulation.mkString("\n"))

  val propSamplesInCircle =
    finalPopulation.foldLeft(0)({
      case (sum, e) =>
        sum + { if (inCircle(vec2Tup(e.sample.values))) 1 else 0 }
    }) /
      finalPopulation.size.toDouble

  println("Proportion of samples in the circle = " ++ propSamplesInCircle.toString)

}

