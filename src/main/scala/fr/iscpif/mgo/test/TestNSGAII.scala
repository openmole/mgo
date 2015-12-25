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

import Contexts.default._
import Contexts._
import Expressions._

import scala.util.Random
import scalaz._
import Scalaz._
import scalaz.effect.IO

object SphereNSGAII extends App {

  import NSGA2.Algorithm.{ Individual, Genome, iGenome, gValues, iFitness }

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val operatorExploration = 0.1

  val fitness: Expression[Vector[Double], Vector[Double]] = { x => Vector(sphere(x)) }

  val algo = NSGA2.Algorithm(mu = mu, lambda = lambda, fitness = fitness, genomeSize = dimensions, operatorExploration = operatorExploration)

  def k[A] = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], A] _
  def ka = Kleisli.ask[EvolutionStateMonad[Unit]#l, Vector[Individual]]

  val ea: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
    runEAUntilStackless[Unit, Individual](
      stopCondition = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Boolean]({ (individuals: Vector[Individual]) =>
        evolutionStateGenerational[Unit].generationReached(maxiter)
      }),
      stepFunction =
        for {
          individuals <- ka
          _ <- writeS(
            { (state: EvolutionData[Unit], individuals: Vector[Individual]) =>
              if (state.generation % 100 == 0)
                individuals.map {
                  i: Individual =>
                    state.generation.toString ++ "\t" ++ (iGenome >=> gValues).get(i).mkString("\t") ++ "\t" ++ iFitness.get(i).mkString("\t")
                }.mkString("\n") ++ "\n"
              else ""
            },
            IO.putStr)
          res <- algo.step
        } yield res,
      start = EvolutionData[Unit](random = newRNG(1), s = ())
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { (g: Genome) => Individual(g, fitness(gValues.get(g))) }
      _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(2)(i => s"f$i").mkString("\t") }.run(Vector.empty)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](evolution)

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: Individual).fitness }.mkString("\n"))
  println("Done")
}

object StochasticSphereNSGAII extends App {

  import NoisyNSGA2.Algorithm.{ Individual, iHistory, iValues }

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val historySize = 10
  val operatorExploration = 0.1
  val cloneProbability = 0.1

  def express: (Random, Vector[Double]) => Vector[Double] = { case (rg: Random, v: Vector[Double]) => Vector(rg.nextGaussian() * 0.5 * math.sqrt(sphere(v))) }

  val algo = NoisyNSGA2.Algorithm(mu = mu, lambda = lambda, fitness = express, operatorExploration = operatorExploration, genomeSize = dimensions, historySize = historySize, cloneProbability = cloneProbability)

  def k[A] = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], A] _
  def ka = Kleisli.ask[EvolutionStateMonad[Unit]#l, Vector[Individual]]

  val ea: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
    runEAUntilStackless[Unit, Individual](
      stopCondition = Kleisli.kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Boolean]({ (individuals: Vector[Individual]) =>
        evolutionStateGenerational[Unit].generationReached(maxiter)
      }),
      stepFunction =
        for {
          _ <- writeS((state: EvolutionData[Unit], individuals: Vector[Individual]) =>
            individuals.map {
              i: Individual => state.generation.toString ++ "\t" ++ iValues.get(i).mkString("\t") ++ "\t" ++ iHistory.get(i).transpose.map { vs => vs.sum / vs.size }.mkString("\t")
            }.mkString("\n"))
          res <- algo.step
        } yield res,
      start = EvolutionData[Unit](random = newRNG(1), s = ())
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { case (rg, i) => iHistory.set(i, Vector(express(rg, i.genome))) }
      _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(2)(i => s"f$i").mkString("\t") ++ "\thistoryLength" }.run(Vector.empty)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](evolution)

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: Individual).fitnessHistory }.mkString("\n"))
}
