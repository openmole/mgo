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

  println(finalpop.map { i => i.genome.values.mkString(",") -> i.fitness.mkString(",") }.mkString("\n"))
}

//object StochasticSphereNSGAII extends App {
//
//  import noisynsga2._
//
//  val mu = 100
//  val lambda = 100
//  def dimensions = 2
//  val maxIter = 10000
//  val historySize = 100
//  val operatorExploration = 0.1
//  val cloneProbability = 0.2
//
//  def express: (Random, Vector[Double]) => Vector[Double] = { case (rg: Random, v: Vector[Double]) => Vector(sphere(v) + rg.nextGaussian() * 0.5 * math.sqrt(sphere(v))) }
//  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }
//
//  val algo =
//    NoisyNSGA2(
//      mu = mu,
//      lambda = lambda,
//      fitness = express,
//      aggregation = aggregation,
//      operatorExploration = operatorExploration,
//      genomeSize = dimensions,
//      historySize = historySize,
//      cloneProbability = cloneProbability)
//
//  val ea: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
//    runEAUntilStackless[Unit, Individual](
//      stopCondition = afterGeneration[EvolutionState[Unit, ?], Individual](maxIter),
//      stepFunction =
//        for {
//          /*_ <- writeS((state: EvolutionData[Unit], individuals: Vector[Individual]) =>
//            individuals.map {
//              i: Individual => state.generation.toString ++ "\t" ++ (Individual.genome composeLens Genome.values).get(i).mkString("\t") ++ "\t" ++ aggregation(Individual.fitnessHistory.get(i)).mkString("\t")
//            }.mkString("\n"))*/
//          res <- algo.step
//        } yield res
//    )
//
//  val evolution: EvolutionState[Unit, Vector[Individual]] =
//    for {
//      gs <- algo.initialGenomes
//      gsRNG <- zipWithRandom[EvolutionState[Unit, ?], Genome](gs)
//      initialPop = gsRNG.map { case (rg, g) => buildIndividual(g, express(rg, vectorValues.get(g))) }
//      //_ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(2)(i => s"f$i").mkString("\t") ++ "\thistoryLength" }.run(Vector.empty)
//      finalpop <- ea.run(initialPop)
//    } yield finalpop
//
//  val (finalstate, finalpop) = algo.run(evolution, new Random(42))
//
//  println("---- Final Population ----")
//  val maxHistory = finalpop.map(_.fitnessHistory.size).max
//  println(finalpop.filter(_.fitnessHistory.size == maxHistory).map(i => (i.genome, aggregation(vectorFitness.get(i)), i.age)).mkString("\n"))
//
//}

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
