/*
 * Copyright (C) Guillaume Chérel 17/10/17
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

object NormalMCSampling extends App {

  import algorithm.monteCarlo.MCSampling._
  import context.implicits._

  def pdfNormal(x: Double): Double = (1.0 / sqrt(2 * Pi)) * exp(-pow(x, 2) / 2.0)

  val mcsampling = MCSampling(
    getSample = rng => Vector(rng.nextGaussian()),
    probability = x => pdfNormal(x.head)
  )

  val (finalState, finalPopulation) =
    run(mcsampling).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  val finalSamples = result(finalPopulation)

  println(finalSamples.mkString("\n"))

  val integral = approxIntegrate(
    x => 1,
    finalSamples)

  println("Integral(1) = " ++ integral.toString)

  println("ArgMax = " ++ approxArgMax(x => pdfNormal(x.head), finalSamples).toString)
}

