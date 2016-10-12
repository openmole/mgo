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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._

object SphereNSGAII extends App {

  import algorithm.nsga2._
  import scala.util.Random

  val nsga2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = x => Vector(sphere(x)),
    genomeSize = 10,
    operatorExploration = 0.1)

  val (finalstate, finalpop) =
    run(nsga2).
      until(stop.afterGeneration(1000)).
      trace((is, s) => println(s.generation)).
      eval(new Random(42))

  println(result(finalpop, sphere.scale).mkString("\n"))

}

object NoisySphereNSGAII extends App {

  import algorithm.noisynsga2._
  import scala.util.Random

  def express(rng: Random, v: Vector[Double]) = Vector(sphere(v) + rng.nextGaussian() * 0.5 * math.sqrt(sphere(v)))
  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  val nsga2 =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = express,
      aggregation = aggregation,
      operatorExploration = 0.1,
      genomeSize = 2,
      historySize = 100,
      cloneProbability = 0.2)

  val (finalstate, finalpop) =
    run(nsga2).
      until(stop.afterGeneration(1000)).
      trace((is, s) => println(s.generation)).
      eval(new Random(42))

  println(result(finalpop, aggregation, sphere.scale).mkString("\n"))

}

object ZDT4NSGAII extends App {

  import algorithm.nsga2._
  import scala.util.Random

  val nsga2 =
    NSGA2(
      mu = 100,
      lambda = 100,
      fitness = zdt4.compute,
      genomeSize = 10)

  val (finalstate, finalpop) =
    run(nsga2).
      until(stop.afterGeneration(1000)).
      trace((is, s) => println(s.generation)).
      eval(new Random(42))

  println(result(finalpop, zdt4.scale).mkString("\n"))

}

