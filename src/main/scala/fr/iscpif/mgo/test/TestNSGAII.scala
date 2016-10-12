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
import fr.iscpif.mgo.algorithm._

import contexts.default._
import contexts._
import stop._

import scala.util.Random
import scalaz._
import Scalaz._

object SphereNSGAII extends App {
  import nsga2._

  val algo = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = x => Vector(sphere(x)),
    genomeSize = 10,
    operatorExploration = 0.1)

  val (finalstate, finalpop) =
    run(algo).
      until(afterGeneration(1000)).
      trace((is, s) => println(s.generation)).
      eval(new Random(42))

  println(
    finalpop.map { i =>
      s"""(${sphere.scale(i.genome.values).mkString(",")}) -> ${i.fitness.mkString(",")}"""
    }.mkString("\n")
  )
}

object StochasticSphereNSGAII extends App {

  import noisynsga2._

  def express(rng: Random, v: Vector[Double]) = Vector(sphere(v) + rng.nextGaussian() * 0.5 * math.sqrt(sphere(v)))
  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  val algo =
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
    run(algo).
      until(afterGeneration(1000)).
      trace((is, s) => println(s.generation)).
      eval(new Random(42))

  //println("---- Final Population ----")
  //val maxHistory = finalpop.map(_.fitnessHistory.size).max
  //println(finalpop.filter(_.fitnessHistory.size == maxHistory).map(i => (i.genome, aggregation(vectorFitness.get(i)), i.age)).mkString("\n"))

}

//object ZDT4NSGAII extends App {
//
//  import nsga2._
//
//  val mu = 100
//  val lambda = 100
//  def dimensions = 10
//  val maxIterations = 1000
//
//  val fitness = (x: Vector[Double]) => zdt4(x)
//
//  val algorithm =
//    NSGA2(
//      mu = mu,
//      lambda = lambda,
//      fitness = fitness,
//      genomeSize = dimensions)
//
//  val ea =
//    runEAUntilStackless[Unit, Individual](
//      stopCondition = afterGeneration[EvolutionState[Unit, ?], Individual](maxIterations),
//      stepFunction = algorithm.step
//    )
//
//  val evolution: EvolutionState[Unit, Vector[Individual]] =
//    for {
//      ig <- algorithm.initialGenomes
//      pop = ig.map { algorithm.expression }
//      finalpop <- ea.run(pop)
//    } yield finalpop
//
//  val (finalstate, finalpop) = algorithm.run(evolution, new Random(42))
//
//  println("---- Fitnesses ----")
//  println(finalpop.map { i => i.genome -> i.fitness }.mkString("\n"))
//
//}
//
