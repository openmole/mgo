/*
 * Copyright (C) Guillaume Chérel 11/08/2017
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
import mgo.contexts._
import freedsl.dsl._
import algorithm.monteCarlo._

object DiskImportanceSampling extends App {

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

  val mcsampling = ImportanceSampling(
    qSample = { r => Vector(r.nextDouble * 2 - 1, r.nextDouble * 2 - 1) },
    qPdf = ifElse(0.25, 0.0) _ compose inSquare compose vec2Tup,
    pPdf = pdfUniformCircle _ compose vec2Tup
  )

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    mcsampling.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).evolution

  val (finalState, finalPopulation) =
    ImportanceSampling(new util.Random(42)) { impl =>
      import impl._
      evolution[DSL].eval
    }

  println(finalPopulation.mkString("\n"))

  val propSamplesInSquare =
    finalPopulation.foldLeft(0)({
      case (sum, e) =>
        sum + { if (inSquare(vec2Tup(e.sample.values))) 1 else 0 }
    }) /
      finalPopulation.size.toDouble

  val propSamplesInCircle =
    finalPopulation.foldLeft(0)({
      case (sum, e) =>
        sum + { if (inCircle(vec2Tup(e.sample.values))) 1 else 0 }
    }) /
      finalPopulation.size.toDouble

  val weightedPropSamplesInCircle =
    finalPopulation.foldLeft(0.0)({
      case (sum, e) =>
        sum + { if (inCircle(vec2Tup(e.sample.values))) e.importance else 0.0 }
    }) /
      finalPopulation.foldLeft(0.0)({ case (sum, e) => sum + e.importance })

  println("Proportion of samples in the square = " ++ propSamplesInSquare.toString)
  println("Proportion of samples in the circle = " ++ propSamplesInCircle.toString ++
    " (Expected: " ++ (Pi / 4.0).toString ++ ")")
  println("Weighted proportion of samples in the circle = " ++ weightedPropSamplesInCircle.toString)

}

