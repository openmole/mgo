package mgo.evolution.algorithm

/*
 * Copyright (C) 2024 Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import cats.implicits.*
import mgo.evolution.*
import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.evolution.ranking.*
import mgo.tools.execution.*
import monocle.*
import monocle.syntax.all.*

import scala.reflect.ClassTag

object HDOSE:

  import CDGenome._
  import DeterministicIndividual._
  import monocle._

  type StateType[P] = Archive[Individual[P]]
  type HDOSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P]: Lens[HDOSEState[P], Archive[Individual[P]]] = Focus[HDOSEState[P]](_.s)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P](
    lambda: Int,
    operatorExploration: Double,
    continuous: Vector[C],
    discrete: Vector[D],
    significanceC: Vector[Double],
    significanceD: Vector[Int],
    diversityDistance: Double,
    fitness: P => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[HDOSEState[P], Individual[P], Genome] =
    HDOSEOperation.adaptiveBreeding[HDOSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      scaledValues(continuous),
      discrete,
      distanceByComponent(significanceC, significanceD),
      diversityDistance,
      buildGenome,
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      archiveLens[P].get)

  def expression[P](fitness: (Vector[Double], Vector[Int]) => P, components: Vector[C]): (Genome, Long, Boolean) => Individual[P] =
    DeterministicIndividual.expression(fitness, components)

  def elitism[P](
    mu: Int,
    limit: Vector[Double],
    significanceC: Vector[Double],
    significanceD: Vector[Int],
    diversityDistance: Double,
    components: Vector[C],
    fitness: P => Vector[Double]): Elitism[HDOSEState[P], Individual[P]] =
    HDOSEOperation.elitism[HDOSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      limit,
      scaledValues(components),
      mu,
      archiveLens[P],
      distanceByComponent(significanceC, significanceD),
      diversityDistance,
      continuousValues.get,
      discreteValues.get,
      Focus[Individual[P]](_.genome).get
    )

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P])

  def result[P](state: HDOSEState[P], population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] =
    val indivduals =
      archiveLens.get(state).toVector ++ (if keepAll then population else Seq())

    indivduals.map: i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), i.focus(_.genome) andThen discreteValues get, DeterministicIndividual.individualFitness(fitness)(i), i)


  def result(ose: HDOSE, state: HDOSEState[Vector[Double]], population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] =
    result[Vector[Double]](state = state, continuous = ose.continuous, fitness = identity, population = population, keepAll = false)

  def reject(ose: HDOSE): Option[Genome => Boolean] = NSGA2.reject(ose.reject, ose.continuous)

  def distanceByComponent(significanceC: Vector[Double], significanceD: Vector[Int]): HDOSEOperation.Distance = (g1, g2) =>
    val (c1, d1) = g1
    val (c2, d2) = g2

    val deltaC =
      val delta = (c1 zip c2).map((c1, c2) => Math.abs(c1 - c2))
      if significanceC.isEmpty then delta.count(_ >= 1.0) else (delta zip significanceC).count((d, s) => d >= s)

    val deltaD =
      val delta = (d1 zip d2).map((c1, c2) => Math.abs(c1 - c2))
      if significanceD.isEmpty then delta.count(_ >= 1.0) else (delta zip significanceD).count((d, s) => d >= s)

    deltaC + deltaD

  given Algorithm[HDOSE, Individual[Vector[Double]], Genome, HDOSEState[Vector[Double]]] with
    override def initialState(t: HDOSE, rng: scala.util.Random) = EvolutionState(s = Archive.empty)

    override def initialPopulation(t: HDOSE, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        HDOSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        HDOSE.expression(t.fitness, t.continuous),
        parallel)

    def step(t: HDOSE) =
      deterministic.step[HDOSEState[Vector[Double]], Individual[Vector[Double]], Genome](
        HDOSE.adaptiveBreeding[Vector[Double]](t.lambda, t.operatorExploration, t.continuous, t.discrete, t.significanceC, t.significanceD, t.diversityDistance, identity, reject(t)),
        HDOSE.expression(t.fitness, t.continuous),
        HDOSE.elitism(t.mu, t.limit, t.significanceC, t.significanceD, t.diversityDistance, t.continuous, identity),
        Focus[HDOSEState[Vector[Double]]](_.generation),
        Focus[HDOSEState[Vector[Double]]](_.evaluated))


case class HDOSE(
  mu: Int,
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  limit: Vector[Double],
  diversityDistance: Double,
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  significanceC: Vector[Double] = Vector.empty,
  significanceD: Vector[Int] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object HDOSEOperation:

  type GenomeValue =  (Vector[Double], Vector[Int])
  type Distance = (GenomeValue, GenomeValue) => Double

  def isTooCloseFromArchive[G, I](
    distance: Distance,
    archive: Archive[I],
    scaledValues: G => (Vector[Double], Vector[Int]),
    genome: I => G,
    diversityDistance: Double)(g: G): Boolean =

    def genomeDistance(g1: G, g2: G): Double =
      distance(scaledValues(g1), scaledValues(g2))

    def tooCloseFromArchive(g: G) =
      archive.exists(i => genomeDistance(genome(i), g) < diversityDistance)

    tooCloseFromArchive(g)

  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    scaledValues: G => (Vector[Double], Vector[Int]),
    discrete: Vector[D],
    distance: Distance,
    diversityDistance: Double,
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    archive: S => Archive[I]): Breeding[S, I, G] =
    (s, population, rng) =>
      val archivedPopulation = archive(s)
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, fitness, rng)
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

      val breeding: Breeding[S, I, G] =
        (s, pop, rng) =>
          val newGs =
            applyDynamicOperators[S, I, G](
              tournament(allRanks, tournamentRounds),
              genome andThen continuousValues,
              genome andThen discreteValues,
              continuousOperatorStatistics,
              discreteOperatorStatistics,
              discrete,
              operatorExploration,
              buildGenome)(s, pop, rng)
          newGs.filterNot:
            isTooCloseFromArchive[G, I](
              distance,
              archivedPopulation,
              scaledValues,
              genome,
              diversityDistance)

      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      randomTake(offspring, lambda, rng)


  def elitism[S, I: ClassTag, G](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    scaledValues: G => (Vector[Double], Vector[Int]),
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    distance: Distance,
    diversityDistance: Double,
    continuousValues: G => Vector[Double],
    discreteValues: G => Vector[Int],
    genome: I => G): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedFitness = mgo.tools.memoize(fitness)
      val cloneRemoved = filterNaN(keepFirst(genome andThen scaledValues)(population, candidates), memoizedFitness)

      // FIXME individuals can be close to each other but yet added to the archive
      def newlyReaching = candidates.filter(c => OSEOperation.patternIsReached(memoizedFitness(c), limit))

      val s2 = archive.modify(_ ++ newlyReaching)(s)
      val filteredPopulation =
        cloneRemoved.filterNot: i =>
          isTooCloseFromArchive(
            distance,
            archive.get(s2),
            scaledValues,
            genome,
            diversityDistance)(genome(i))

      NSGA2Operations.elitism[S, I](memoizedFitness, genome andThen scaledValues, mu)(s2, filteredPopulation, Vector.empty, rng)



