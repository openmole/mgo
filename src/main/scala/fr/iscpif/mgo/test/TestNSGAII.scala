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
import fitness._
import genome._

import Contexts.default._
import Contexts._
import Expressions._

import scala.util.Random
import scalaz._
import Scalaz._

import scalaz.effect.IO

object SphereNSGAII extends App {

  import NSGA2.Algorithm.{ Individual, Genome, gValues }

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val operatorExploration = 0.1

  val fitness: Expression[Vector[Double], Vector[Double]] = { x => Vector(sphere(x)) }

  val algo = NSGA2.Algorithm(mu = mu, lambda = lambda, fitness = fitness, genomeSize = dimensions, operatorExploration = operatorExploration)

  val ea: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
    runEAUntil[EvolutionStateMonad[Unit]#l, Individual](
      stopCondition = { (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[Individual]) => for {
          _ <- liftIOValue[Unit, Unit](writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue[Unit, Unit](write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitness.sum }.toString))
          res <- algo.step(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { (g: Genome) => Individual(g, fitness(gValues.get(g))) }
      finalpop <- ea(initialPop)
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
  println(finalpop.map { (_: Individual).fitness }.mkString("\n"))
  println("Done")
}

object StochasticSphereNSGAII extends App {

  import NoisyNSGA2.Algorithm.{ Individual, iHistory }

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val historySize = 10
  val operatorExploration = 0.1
  val cloneProbability = 0.1

  def express: (Random, Vector[Double]) => Vector[Double] = { case (rg: Random, v: Vector[Double]) => Vector(rg.nextGaussian() * 0.5 * math.sqrt(sphere(v))) }

  val algo = NoisyNSGA2.Algorithm(mu = mu, lambda = lambda, fitness = express, operatorExploration = operatorExploration, genomeSize = dimensions, historySize = historySize, cloneProbability = cloneProbability)

  val ea: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
    runEAUntil[EvolutionStateMonad[Unit]#l, Individual](
      stopCondition = { (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[Individual]) => for {
          _ <- liftIOValue[Unit, Unit](writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue[Unit, Unit](write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitnessHistory.last.sum }.toString))
          res <- algo.step(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { case (rg, i) => iHistory.set(i, Vector(express(rg, i.genome))) }
      finalpop <- ea(initialPop)
    } yield finalpop

  val start = algo.wrap[Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](start >> evolution)

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: Individual).fitnessHistory }.mkString("\n"))
}
