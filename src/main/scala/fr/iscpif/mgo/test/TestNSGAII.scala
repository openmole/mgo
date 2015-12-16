/*
 * Copyright (C) 2012 Romain Reuillon
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

import algorithm.NSGA2
import Contexts.default._
import Contexts._
import Expressions._

import scala.util.Random
import scalaz._
import Scalaz._

import scalaz.effect.IO

object SphereNSGAII extends App {

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 500
  val operationExploration = 0.1

  def express: Expression[NSGA2.Genome, Vector[Double]] = { case NSGA2.Genome(values, _, _) => Vector(sphere(values)) }

  val ea: Vector[NSGA2.Individual] => EvolutionState[Unit, Vector[NSGA2.Individual]] =
    runEAUntil[NSGA2.Individual, EvolutionStateMonad[Unit]#l](
      stopCondition = { (individuals: Vector[NSGA2.Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[NSGA2.Individual]) => for {
          _ <- liftIOValue[Unit, Unit](writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue[Unit, Unit](write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitness.sum }.toString))
          res <- NSGA2.step[EvolutionStateMonad[Unit]#l](
            fitness = express,
            mu = mu,
            lambda = lambda,
            operationExploration = operationExploration)(implicitly[Monad[EvolutionStateMonad[Unit]#l]], implicitly[RandomGen[EvolutionStateMonad[Unit]#l]], implicitly[Generational[EvolutionStateMonad[Unit]#l]])(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[NSGA2.Individual]] =
    for {
      initialGenomes <- NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l](mu, dimensions)
      initialPop = initialGenomes.map { (g: NSGA2.Genome) => NSGA2.Individual(g, express(g)) }
      finalpop <- ea(initialPop)
    } yield finalpop

  val start = Contexts.default.wrap[Unit, Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = Contexts.default.unwrap[Unit, Vector[NSGA2.Individual]](s = ())(
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: NSGA2.Individual).fitness }.mkString("\n"))
}

object StochasticSphereNSGAII extends App {

  val mu = 10
  val lambda = 10
  def dimensions = 10
  val maxiter = 100
  val historySize = 10
  val operationExploration = 0.1
  val cloneProbability = 0.1

  def express: Expression[(Random, Vector[Double]), Vector[Double]] = { case (rg: Random, v: Vector[Double]) => Vector(rg.nextGaussian() * 0.5 * math.sqrt(sphere(v))) }

  val ea: Vector[NoisyNSGA2.Individual] => EvolutionState[Unit, Vector[NoisyNSGA2.Individual]] =
    runEAUntil[NoisyNSGA2.Individual, EvolutionStateMonad[Unit]#l](
      stopCondition = { (individuals: Vector[NoisyNSGA2.Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[NoisyNSGA2.Individual]) => for {
          _ <- liftIOValue[Unit, Unit](writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue[Unit, Unit](write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitnessHistory.last.sum }.toString))
          res <- NoisyNSGA2.step[EvolutionStateMonad[Unit]#l](
            fitness = express,
            mu = mu,
            lambda = lambda,
            historySize = historySize,
            operationExploration = operationExploration,
            cloneProbability = cloneProbability)(implicitly[Monad[EvolutionStateMonad[Unit]#l]], implicitly[RandomGen[EvolutionStateMonad[Unit]#l]], implicitly[Generational[EvolutionStateMonad[Unit]#l]])(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[NoisyNSGA2.Individual]] =
    for {
      rg <- implicitly[RandomGen[EvolutionStateMonad[Unit]#l]].split
      initialGenomes <- NoisyNSGA2.initialPopulation[EvolutionStateMonad[Unit]#l](mu, dimensions)
      initialPop = initialGenomes.map { (i: NoisyNSGA2.Individual) => i.copy(fitnessHistory = Vector(express((rg, i.genome)))) }
      finalpop <- ea(initialPop)
    } yield finalpop

  val start = Contexts.default.wrap[Unit, Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = Contexts.default.unwrap[Unit, Vector[NoisyNSGA2.Individual]](s = ())(
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: NoisyNSGA2.Individual).fitnessHistory }.mkString("\n"))
}

/*object SphereNSGAIIOld extends App {

  def dimensions = 10
  def problem(g: GAGenome) = State { rng: Random => (rng, sphere(g.genomeValue)) }

  val evo =
    evolution(
      NSGA2[Double](
        mu = 10,
        fitness = i => Seq(i.phenotype)
      )
    )(
        100,
        randomGenome(dimensions),
        problem,
        afterGeneration(100)
      )

  println(evo.eval(42).minBy(_.phenotype))

}*/

/*object StochasticSphereNSGAIIOld extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val algo =
    noisyNSGA2[Double](
      mu = 100,
      fitness = i => Seq(average(i.phenotype)),
      history = 100
    )

  def dimensions = 10
  def function = rastrigin

  def problem(g: GAGenome) = State { rng: Random =>
    val scaled = function.scale(g.genomeValue)
    val eval = function.compute(scaled)
    (rng, eval + (rng.nextGaussian() * 0.5 * math.sqrt(eval)))
  }

  val evo = evolution(algo)(100, randomGenome(dimensions), problem, afterGeneration(10000))

  import scala.Ordering.Implicits._
  val (s, res) = evo.run(47)

  val oldest = res.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))

  println(res.count(_.phenotype.size == oldest.phenotype.size))
  println(res.groupBy(_.genome.fromOperation).toList.map { case (k, v) => k -> v.size }.sortBy(_._1))
  println(function.scale(oldest.genomeValue) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size + " " + oldest.born)

}*/
