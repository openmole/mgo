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
import mgo.tools.MonteCarlo._

object DiskRejectionSampling extends App {

  import algorithm.monteCarlo.RejectionSampling._
  import context.implicits._

  def pdfUniformCircle(x: (Double, Double)): Double =
    if (sqrt(pow(x._1, 2) + pow(x._2, 2)) <= 1)
      1.0 / Pi
    else
      0.0

  def inSquare(x: (Double, Double)): Boolean =
    x._1 >= -1 && x._1 < 1 && x._2 >= -1 && x._2 < 1

  def inBottomLeftQuarter(x: (Double, Double)): Boolean =
    x._1 < 0.0 && x._2 < 0.0
  def inTopLeftQuarter(x: (Double, Double)): Boolean =
    x._1 >= 0.0 && x._2 < 0.0
  def inTopRightQuarter(x: (Double, Double)): Boolean =
    x._1 >= 0.0 && x._2 >= 0.0
  def inBottomRightQuarter(x: (Double, Double)): Boolean =
    x._1 < 0.0 && x._2 >= 0.0

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

  val finalSamples: Vector[Vector[Double]] = result(finalPopulation)

  println(finalSamples.mkString("\n"))

  val propSamplesInCircle = approxIntegrate(
    ifElse(1.0, 0.0) _ compose inSquare _ compose vec2Tup _,
    finalSamples)

  val propSamplesOutsideCircle = approxIntegrate(
    ifElse(0.0, 1.0) _ compose inSquare _ compose vec2Tup _,
    finalSamples)

  val propSamplesBottomLeftQuarter = approxIntegrate(
    ifElse(1.0, 0.0) _ compose inBottomLeftQuarter _ compose vec2Tup _,
    finalSamples)
  val propSamplesTopLeftQuarter = approxIntegrate(
    ifElse(1.0, 0.0) _ compose inTopLeftQuarter _ compose vec2Tup _,
    finalSamples)
  val propSamplesBottomRightQuarter = approxIntegrate(
    ifElse(1.0, 0.0) _ compose inBottomRightQuarter _ compose vec2Tup _,
    finalSamples)
  val propSamplesTopRightQuarter = approxIntegrate(
    ifElse(1.0, 0.0) _ compose inTopRightQuarter _ compose vec2Tup _,
    finalSamples)

  println("Proportion of samples in the circle = " ++ propSamplesInCircle.toString)
  println("Proportion of samples outside the circle = " ++ propSamplesOutsideCircle.toString)
  println("Proportion of samples in the bottom left quarter = " ++ propSamplesBottomLeftQuarter.toString)
  println("Proportion of samples in the top left quarter = " ++ propSamplesTopLeftQuarter.toString)
  println("Proportion of samples in the bottom right quarter = " ++ propSamplesBottomRightQuarter.toString)
  println("Proportion of samples in the top right quarter = " ++ propSamplesTopRightQuarter.toString)

}

