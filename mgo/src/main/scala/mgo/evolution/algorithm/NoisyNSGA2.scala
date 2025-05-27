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
import mgo.tools._

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

object NoisyNSGA2 {

  import CDGenome._
  import NoisyIndividual._

  type NSGA2State = EvolutionState[Unit]

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int, individual: Individual[P])

  def result[P: Manifest](population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], discrete: Vector[D], keepAll: Boolean): Vector[Result[P]] =
    val individuals = if (keepAll) population else keepFirstFront(population, fitness(aggregation))

    individuals.map: i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous, discrete)
      Result(c, d, f, r, i)


  def result[P: Manifest](nsga2: NoisyNSGA2[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result[P](population, nsga2.aggregation, nsga2.continuous, nsga2.discrete, keepAll = false)

  def fitness[P: Manifest](aggregation: Vector[P] => Vector[Double]): Individual[P] => Vector[Double] =
    NoisyNSGA2Operations.aggregated[Individual[P], P](
      vectorPhenotype[P].get,
      aggregation,
      _.phenotypeHistory.size)(_)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    continuous: Vector[C],
    discrete: Vector[D],
    reject: Option[Genome => Boolean]): Breeding[S, Individual[P], Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[S, Individual[P], Genome, P](
      fitness(aggregation),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      buildGenome(discrete),
      _ => 1,
      lambda,
      reject,
      operatorExploration,
      cloneProbability)

  def expression[P: Manifest](phenotype: (util.Random, IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    NoisyIndividual.expression[P](phenotype, continuous, discrete)

  def elitism[S, P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], discrete: Vector[D]): Elitism[S, Individual[P]] =
    def individualValues(i: Individual[P]) = scaledVectorValues(components, discrete)(i.genome)

    NoisyNSGA2Operations.elitism[S, Individual[P], P](
      fitness[P](aggregation),
      mergeHistories(individualValues, vectorPhenotype[P], Focus[Individual[P]](_.historyAge), historySize),
      mu)

  def reject[P](nsga2: NoisyNSGA2[P]): Option[Genome => Boolean] = NSGA2.reject(nsga2.reject, nsga2.continuous, nsga2.discrete)

  given isAlgorithm[P: Manifest]: Algorithm[NoisyNSGA2[P], Individual[P], Genome, NSGA2State] with
    def initialState(t: NoisyNSGA2[P], rng: scala.util.Random) = EvolutionState(s = ())

    def initialPopulation(t: NoisyNSGA2[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyNSGA2.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyNSGA2.expression[P](t.fitness, t.continuous, t.discrete),
        rng,
        parallel)

    def step(t: NoisyNSGA2[P]) =
      noisy.step[NSGA2State, Individual[P], Genome](
        NoisyNSGA2.adaptiveBreeding[NSGA2State, P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.continuous,
          t.discrete,
          reject(t)),
        NoisyNSGA2.expression(t.fitness, t.continuous, t.discrete),
        NoisyNSGA2.elitism[NSGA2State, P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.discrete),
        Focus[NSGA2State](_.generation),
        Focus[NSGA2State](_.evaluated))

}

case class NoisyNSGA2[P](
  mu: Int,
  lambda: Int,
  fitness: (util.Random, IArray[Double], IArray[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object NoisyNSGA2Operations:

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double], accuracy: I => Double)(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / accuracy(i))

  def adaptiveBreeding[S, I, G, P](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    cloneProbability: Double): Breeding[S, I, G] =
    (s, population, rng) =>
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity(population, fitness, rng)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      val genomeValue = genome andThen (continuousValues, discreteValues).tupled

      val breeding = applyDynamicOperators[S, I, G](
        tournament(ranks, tournamentRounds),
        genomeValue,
        continuousOperatorStatistics,
        discreteOperatorStatistics,
        discrete,
        operatorExploration,
        buildGenome)

      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population, rng)
      val sizedOffspringGenomes = randomTake(offspring, lambda, rng)
      clonesReplace(cloneProbability, population, genome, tournament(ranks, tournamentRounds))(s, sizedOffspringGenomes, rng)


  def elitism[S, I, P](
    fitness: I => Vector[Double],
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedFitness = fitness.memoized
      val merged = filterNaN(mergeHistories(population, candidates), memoizedFitness)
      val ranks = paretoRankingMinAndCrowdingDiversity[I](merged, memoizedFitness, rng)
      (s, keepHighestRanked(merged, ranks, mu, rng))



