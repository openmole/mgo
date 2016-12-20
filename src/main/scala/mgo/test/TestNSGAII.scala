/*
 * Copyright (C) 2012 Guillaume Chérel, Romain Reuillon
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

import mgo._

object SphereNSGAII extends App {

  import algorithm.nsga2._
  import context.implicits._

  val nsga2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = (v: Vector[Double]) => Vector(sphere.compute(v)),
    genomeSize = 2,
    operatorExploration = 0.1)

  val (finalState, finalPopulation) =
    run(nsga2).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  println(result(finalPopulation, sphere.scale).mkString("\n"))

}

object NoisySphereNSGAII extends App {

  import algorithm.noisynsga2._
  import context.implicits._

  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  val nsga2 =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = (rng, v) => Vector(noisySphere.compute(rng, v)),
      aggregation = aggregation,
      genomeSize = 2)

  val (finalState, finalPopulation) =
    run(nsga2).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  println(result(finalPopulation, aggregation, noisySphere.scale).mkString("\n"))

}

object ZDT4NSGAII extends App {

  import algorithm.nsga2._
  import context.implicits._

  val nsga2 =
    NSGA2(
      mu = 100,
      lambda = 100,
      fitness = zdt4.compute,
      genomeSize = 10)

  val (finalState, finalPopulation) =
    run(nsga2).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

}

