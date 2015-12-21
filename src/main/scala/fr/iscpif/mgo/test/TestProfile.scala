/*
 * Copyright (C) 08/01/13 Guillaume Chérel, Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import fr.iscpif.mgo.algorithm._
import fr.iscpif.mgo.niche._

import Contexts.default._
import Contexts._
import Expressions._

import scala.util.Random
import scalaz._
import Scalaz._

import scalaz.effect.IO

object SphereProfile extends App {

  import Profile.Algorithm.{ Individual, Genome, iGenome, gValues, iFitness }

  val muByNiche = 1
  val lambda = 10
  val dimensions = 10
  val maxiter = 100
  val operatorExploration = 0.1

  def express: Vector[Double] => Double = sphere(_)

  //Niche over the first dimension of the genome
  def niche: Niche[Individual, Int] = genomeProfile[Individual](
    values = (iGenome >=> gValues).get,
    x = 0,
    nX = 10)

  val algo = Profile.Algorithm(
    muByNiche = muByNiche,
    lambda = lambda,
    fitness = express,
    niche = niche,
    genomeSize = dimensions,
    operatorExploration = operatorExploration)

  def k[A] = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], A] _
  def ka = Kleisli.ask[EvolutionStateMonad[Unit]#l, Vector[Individual]]

  val ea: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
    runEAUntil[EvolutionStateMonad[Unit]#l, Individual](
      stopCondition = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Boolean]({ (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      }),
      stepFunction =
        for {
          _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) =>
            individuals.map {
              i: Individual => state.generation.toString ++ "\t" ++ (iGenome >=> gValues).get(i).mkString("\t") ++ "\t" ++ iFitness.get(i).toString
            }.mkString("\n")
          }
          res <- algo.step
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { (g: Genome) => Individual(g, express(gValues.get(g))) }
      _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ "fitness" }.run(Vector.empty)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val start = algo.wrap[Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { iFitness.get(_: Individual) }.mkString("\n"))

}

object StochasticSphereProfile extends App {

  import NoisyProfile.Algorithm.{ Individual, iHistory, iFitness, iValues }

  val muByNiche = 1
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val operatorExploration = 0.1
  val cloneProbability = 0.1
  val historySize = 100

  //TODO: Stochastic Sphere
  def express: (Random, Vector[Double]) => Double = { case (rg: Random, v: Vector[Double]) => rg.nextGaussian() * 0.5 + math.sqrt(sphere(v)) }

  //Niche over the first dimension of the genome
  def niche: Niche[Individual, Int] = genomeProfile[Individual](
    values = iValues.get(_: Individual),
    x = 0,
    nX = 10)

  val algo = NoisyProfile.Algorithm(
    muByNiche = muByNiche,
    lambda = lambda,
    fitness = express,
    niche = niche,
    genomeSize = dimensions,
    historySize = historySize,
    operatorExploration = operatorExploration,
    cloneProbability = cloneProbability)

  def k[A] = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], A] _
  def ka = Kleisli.ask[EvolutionStateMonad[Unit]#l, Vector[Individual]]

  val ea: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
    runEAUntil[EvolutionStateMonad[Unit]#l, Individual](
      stopCondition = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Boolean]({ (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      }),
      stepFunction =
        for {
          individuals <- ka
          _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) =>
            individuals.map {
              i: Individual => state.generation.toString ++ "\t" ++ iValues.get(i).mkString("\t") ++ "\t" ++ iFitness.get(i).toString ++ "\t" ++ iHistory.get(i).length.toString
            }.mkString("\n")
          }
          res <- algo.step
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPopEval = ig.map { case (rg, i) => iHistory.set(i, Vector(express(rg, i.genome))) }
      _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ "fitness" ++ "\t" ++ "historyLength" }.run(Vector.empty)
      finalpop <- ea.run(initialPopEval)
    } yield finalpop

  val start = algo.wrap[Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { iHistory.get(_) }.mkString("\n"))

}
