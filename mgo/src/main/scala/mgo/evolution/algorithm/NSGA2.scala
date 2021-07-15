/*
 * Copyright (C) 15/12/2015 Guillaume Chérel
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
package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.execution._
import monocle.Focus
import monocle.syntax.all._

import scala.language.higherKinds

object NSGA2 {

  import CDGenome._
  import DeterministicIndividual._

  type NSGA2State = EvolutionState[Unit]

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P](lambda: Int, operatorExploration: Double, discrete: Vector[D], fitness: P => Vector[Double], reject: Option[Genome => Boolean]): Breeding[S, Individual[P], Genome] =
    NSGA2Operations.adaptiveBreeding[S, Individual[P], Genome](
      individualFitness[P](fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      _ => 1,
      lambda,
      reject,
      operatorExploration)

  def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(express, components)

  def elitism[S, P](mu: Int, components: Vector[C], fitness: P => Vector[Double]): Elitism[S, Individual[P]] =
    NSGA2Operations.elitism[S, Individual[P]](
      individualFitness[P](fitness),
      i => values(i.focus(_.genome).get, components),
      mu)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P])

  def reject(f: Option[(Vector[Double], Vector[Int]) => Boolean], continuous: Vector[C]): Option[Genome => Boolean] =
    f.map { reject => (g: Genome) =>
      val scaledContinuous = scaleContinuousValues(continuousValues.get(g), continuous)
      val discreteValue = discreteValues get g
      reject(scaledContinuous, discreteValue)
    }

  def result[P](population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] = {
    val individuals = if (keepAll) population else keepFirstFront(population, individualFitness(fitness))
    individuals.map { i => Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), i.focus(_.genome) andThen discreteValues get, individualFitness(fitness)(i), i) }
  }

  implicit def isAlgorithm: Algorithm[NSGA2, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2, Individual[Vector[Double]], Genome, NSGA2State] {
      override def initialState(t: NSGA2, rng: scala.util.Random) = EvolutionState(s = ())
      override def initialPopulation(t: NSGA2, rng: scala.util.Random) =
        deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
          NSGA2.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2) =
        (s, population, rng) =>
          deterministic.step[NSGA2State, Individual[Vector[Double]], Genome](
            NSGA2.adaptiveBreeding[NSGA2State, Vector[Double]](t.lambda, t.operatorExploration, t.discrete, identity, reject(t)),
            NSGA2.expression(t.fitness, t.continuous),
            NSGA2.elitism[NSGA2State, Vector[Double]](t.mu, t.continuous, identity),
            Focus[EvolutionState[Unit]](_.generation),
            Focus[EvolutionState[Unit]](_.evaluated))(s, population, rng)
    }

  def result(nsga2: NSGA2, population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] = result[Vector[Double]](population, nsga2.continuous, identity[Vector[Double]] _, keepAll = false)
  def reject(nsga2: NSGA2): Option[Genome => Boolean] = reject(nsga2.reject, nsga2.continuous)

}

case class NSGA2(
  mu: Int,
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object NSGA2Operations {

  //  def breeding[M[_]: cats.Monad: Generation: Random, I, G](
  //    fitness: I => Vector[Double],
  //    genome: I => G,
  //    genomeValues: G => Vector[Double],
  //    buildGenome: (Vector[Double], Vector[Long]) => G)(crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, I, G] = Breeding { population =>
  //    for {
  //      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
  //      breeding = applyOperators[M, I, Vector[Double]](crossover, mutation, tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), genome andThen genomeValues) apply population
  //      offspring <- breeding repeat ((lambda + 1) / 2)
  //      offspringGenomes = offspring.flatMap {
  //        case (o1, o2) =>
  //          def gv1 = o1.map(math.clamp(_))
  //          def gv2 = o2.map(math.clamp(_))
  //          Vector(buildGenome(gv1), buildGenome(gv2))
  //      }
  //      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
  //    } yield sizedOffspringGenomes
  //  }

  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double): Breeding[S, I, G] =
    (s, population, rng) => {
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, fitness, rng)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      val breeding: Breeding[S, I, G] = applyDynamicOperators[S, I, G](
        tournament(ranks, tournamentRounds),
        genome andThen continuousValues,
        genome andThen discreteValues,
        continuousOperatorStatistics,
        discreteOperatorStatistics,
        discrete,
        operatorExploration,
        buildGenome)

      val offspring = breed(breeding, lambda, reject)(s, population, rng)
      randomTake(offspring, lambda, rng)
    }

  def elitism[S, I](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    mu: Int): Elitism[S, I] =
    (s, population, candidates, rng) => {
      val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), fitness)
      val ranks = paretoRankingMinAndCrowdingDiversity[I](cloneRemoved, fitness, rng)
      (s, keepHighestRanked(cloneRemoved, ranks, mu, rng))
    }

}
