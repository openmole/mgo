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

import algorithm.monteCarlo.SimpleMCSampling
import algorithm.monteCarlo.MCSampling.context.implicits._

object NormalSimpleMCSampling extends App {

  def pdfNormal(x: Double): Double = (1.0 / sqrt(2 * Pi)) * exp(-pow(x, 2) / 2.0)

  val mcsampling = SimpleMCSampling(
    sample = rng => Vector(rng.nextGaussian()),
    probability = x => pdfNormal(x.head)
  )

  val (finalState, finalPopulation) =
    run(mcsampling).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  val finalSamples: Vector[Vector[Double]] = SimpleMCSampling.result(finalPopulation)

  println(finalSamples.mkString("\n"))

  val integral = finalPopulation.map({
    e => if (e.sample.values.head < 2 && e.sample.values.head > -2) 1.0 else 0.0
  }).sum / finalPopulation.size.toDouble

  def approxArgMax(p: Vector[Double] => Double, samples: Vector[Vector[Double]]): Vector[Double] =
    samples.maxBy(p)

  val argmax = finalPopulation.maxBy({ e => e.sample.values.head })

  println("Proportion of samples within 2 standard deviation = " ++ integral.toString)

  println("Sample with the highest probability = " ++ argmax.toString)
}

