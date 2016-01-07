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
import expressions._
import stop._

import scala.util.Random
import scalaz._
import Scalaz._
import scalaz.effect.IO

object SphereNSGAII extends App {

  import NSGA2.Algorithm._

  val mu = 100
  val lambda = 100
  def dimensions = 10
  val maxIter = 1000
  val operatorExploration = 0.1

  val fitness: Expression[Vector[Double], Vector[Double]] = { x => Vector(sphere(x)) }

  val algo = NSGA2.Algorithm(mu = mu, lambda = lambda, fitness = fitness, genomeSize = dimensions, operatorExploration = operatorExploration)

  def ka = Kleisli.ask[EvolutionState[Unit, ?], Vector[Individual]]

  val ea: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
    runEAUntilStackless[Unit, Individual](
      stopCondition = afterGeneration[EvolutionState[Unit, ?], Individual](maxIter),
      stepFunction =
        for {
          //individuals <- ka
          /*_ <- writeS(
            { (state: EvolutionData[Unit], individuals: Vector[Individual]) =>
              if (state.generation % 100 == 0)
                individuals.map {
                  i: Individual =>
                    state.generation.toString ++ "\t" ++ (Individual.genome composeLens Genome.values).get(i).mkString("\t") ++ "\t" ++ Individual.fitness.get(i).mkString("\t")
                }.mkString("\n") ++ "\n"
              else ""
            },
            IO.putStr)*/
          res <- algo.step
        } yield res,
      start = EvolutionData[Unit](random = newRNG(1), s = ())
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { algo.expression }
      // _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(2)(i => s"f$i").mkString("\t") }.run(Vector.empty)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](evolution)

  println("---- Fitnesses ----")
  println(finalpop.map { i => i.genome -> i.fitness }.mkString("\n"))

}

object StochasticSphereNSGAII extends App {

  import NoisyNSGA2.Algorithm._

  val mu = 100
  val lambda = 100
  def dimensions = 2
  val maxIter = 10000
  val historySize = 100
  val operatorExploration = 0.1
  val cloneProbability = 0.2

  def express: (Random, Vector[Double]) => Vector[Double] = { case (rg: Random, v: Vector[Double]) => Vector(sphere(v) + rg.nextGaussian() * 0.5 * math.sqrt(sphere(v))) }
  def aggregation(history: Vector[Vector[Double]]) =
    history.transpose.map { o => o.sum / o.size }

  val algo =
    NoisyNSGA2.Algorithm(
      mu = mu,
      lambda = lambda,
      fitness = express,
      aggregation = aggregation,
      operatorExploration = operatorExploration,
      genomeSize = dimensions,
      historySize = historySize,
      cloneProbability = cloneProbability)

  val ea: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
    runEAUntilStackless[Unit, Individual](
      stopCondition = afterGeneration[EvolutionState[Unit, ?], Individual](maxIter),
      stepFunction =
        for {
          /*_ <- writeS((state: EvolutionData[Unit], individuals: Vector[Individual]) =>
            individuals.map {
              i: Individual => state.generation.toString ++ "\t" ++ (Individual.genome composeLens Genome.values).get(i).mkString("\t") ++ "\t" ++ aggregation(Individual.fitnessHistory.get(i)).mkString("\t")
            }.mkString("\n"))*/
          res <- algo.step
        } yield res,
      start = EvolutionData[Unit](random = newRNG(1), s = ())
    )

  def zipWithRandom[M[_]: Monad, G](gs: Vector[G])(implicit MR: ParallelRandomGen[M]): M[Vector[(Random, G)]] =
    for {
      rngs <- MR.split.replicateM(gs.size)
    } yield rngs.toVector zip gs

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      gs <- algo.initialGenomes
      gsRNG <- zipWithRandom[EvolutionState[Unit, ?], Genome](gs)
      initialPop = gsRNG.map { case (rg, g) => buildIndividual(g, express(rg, Genome.values.get(g))) }
      //_ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(2)(i => s"f$i").mkString("\t") ++ "\thistoryLength" }.run(Vector.empty)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](evolution)

  println("---- Final Population ----")
  val maxHistory = finalpop.map(_.fitnessHistory.size).max
  println(finalpop.filter(_.fitnessHistory.size == maxHistory).map(i => (i.genome, aggregation(i.fitnessHistory), i.born)).mkString("\n"))

}
