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


import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.algorithm.OSEOperation.ReachMap
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.execution._

import monocle.function
import monocle._
import monocle.syntax.all._

import scala.reflect.ClassTag

object NoisyHDOSE:
  import CDGenome.*
  import NoisyIndividual.*

  case class StateType[P](archive: Archive[Individual[P]], distance: Double)
  type HDOSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P]: Lens[EvolutionState[StateType[P]], Archive[Individual[P]]] = Focus[EvolutionState[StateType[P]]](_.s.archive)
  def distanceLens[P]: Lens[HDOSEState[P], Double] = Focus[HDOSEState[P]](_.s.distance)

  def initialState[P](distance: Double = 1.0): HDOSEState[P] = EvolutionState(s = StateType(Archive.empty, distance))

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    continuous: Vector[C],
    discrete: Vector[D],
    significanceC: Vector[Double],
    significanceD: Vector[Int],
    limit: Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[HDOSEState[P], Individual[P], Genome] =
    NoisyHDOSEOperations.adaptiveBreeding[HDOSEState[P], Individual[P], Genome, P](
      vectorPhenotype[P].get,
      aggregation,
      Focus[Individual[P]](_.genome).get,
      continuousVectorValues.get,
      continuousOperator.get,
      discreteVectorValues.get,
      discreteOperator.get,
      discrete,
      scaledValues(continuous),
      HDOSE.toCloseByComponent(significanceC, significanceD),
      distanceLens.get,
      buildGenome,
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      cloneProbability,
      limit,
      archiveLens.get)

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]) =
    NoisyIndividual.expression[P](fitness, continuous)

  def elitism[P: Manifest](
    mu: Int,
    historySize: Int,
    aggregation: Vector[P] => Vector[Double],
    components: Vector[C],
    significanceC: Vector[Double],
    significanceD: Vector[Int],
    archiveSize: Int,
    limit: Vector[Double]): Elitism[HDOSEState[P], Individual[P]] =
    def individualValues(i: Individual[P]) = scaledVectorValues(components)(i.genome)

    NoisyHDOSEOperations.elitism[HDOSEState[P], Individual[P], P, Genome](
      _.genome,
      vectorPhenotype[P].get,
      aggregation,
      scaledValues(components),
      limit,
      historySize,
      mergeHistories(individualValues, vectorPhenotype[P], Focus[Individual[P]](_.historyAge), historySize),
      mu,
      archiveLens,
      HDOSE.toCloseByComponent(significanceC, significanceD),
      distanceLens,
      archiveSize
    )


  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int, individual: Individual[P])

  def result[P: Manifest](state: HDOSEState[P], population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], limit: Vector[Double], keepAll: Boolean): Vector[Result[P]] =
    def goodIndividuals =
      population.flatMap: i =>
        val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous)
        if (keepAll || OSEOperation.patternIsReached(f, limit))
        then Some(Result(c, d, f, r, i))
        else None

    state.s.archive.toVector.map: i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
      Result(c, d, f, r, i)
    ++ goodIndividuals

  def result[P: Manifest](noisyHDOSE: NoisyHDOSE[P], state: HDOSEState[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result[P](state, population, noisyHDOSE.aggregation, noisyHDOSE.continuous, noisyHDOSE.limit, keepAll = false)

  def reject[P](pse: NoisyHDOSE[P]): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous)

  given [P: Manifest]: Algorithm[NoisyHDOSE[P], Individual[P], Genome, HDOSEState[P]] with
    def initialState(t: NoisyHDOSE[P], rng: scala.util.Random) = NoisyHDOSE.initialState(t.distance)
    
    def initialPopulation(t: NoisyHDOSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyOSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyOSE.expression[P](t.fitness, t.continuous),
        rng,
        parallel)

    def step(t: NoisyHDOSE[P]) =
      val sC = t.significanceC.getOrElse(Vector.fill(t.continuous.size)(1.0))
      val sD = t.significanceD.getOrElse(Vector.fill(t.discrete.size)(1))

      noisy.step[HDOSEState[P], Individual[P], Genome](
        NoisyHDOSE.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.continuous,
          t.discrete,
          sC,
          sD,
          t.limit,
          reject(t)),
        NoisyHDOSE.expression(t.fitness, t.continuous),
        NoisyHDOSE.elitism[P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          sC,
          sD,
          t.archiveSize,
          t.limit),
        Focus[HDOSEState[P]](_.generation),
        Focus[HDOSEState[P]](_.evaluated)
      )



case class NoisyHDOSE[P](
  mu: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => P,
  limit: Vector[Double],
  archiveSize: Int,
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  significanceC: Option[Vector[Double]] = None,
  significanceD: Option[Vector[Int]] = None,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None,
  distance: Double = 1.0)

object NoisyHDOSEOperations:


  def adaptiveBreeding[S, I: ClassTag, G, P](
                                              history: I => Vector[P],
                                              aggregation: Vector[P] => Vector[Double],
                                              genome: I => G,
                                              continuousValues: G => Vector[Double],
                                              continuousOperator: G => Option[Int],
                                              discreteValues: G => Vector[Int],
                                              discreteOperator: G => Option[Int],
                                              discrete: Vector[D],
                                              scaledValues: G => (IArray[Double], IArray[Int]),
                                              distance: HDOSEOperation.TooClose,
                                              diversityDistance: S => Double,
                                              buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
                                              tournamentRounds: Int => Int,
                                              lambda: Int,
                                              reject: Option[G => Boolean],
                                              operatorExploration: Double,
                                              cloneProbability: Double,
                                              limit: Vector[Double],
                                              archive: S => Archive[I]): Breeding[S, I, G] =
    (s, population, rng) =>
      val archivedPopulation = archive(s)

      def fitness = NoisyOSEOperations.aggregated(history, aggregation)
      val memoizedFitness = mgo.tools.memoize(fitness)

      def promising: Vector[I] =
        population.filter(i => OSEOperation.patternIsReached(memoizedFitness(i), limit))

      val tooCloseFromArchiveOrPromising =
        HDOSEOperation.isTooCloseFromArchive[G, I](
          distance,
          archivedPopulation ++ promising,
          scaledValues,
          genome,
          diversityDistance(s))

      def filterTooCloseFromArchiveOrPromising(genomes: Vector[G]) = genomes.filterNot(tooCloseFromArchiveOrPromising)

      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, memoizedFitness, rng)
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

      def breeding: Breeding[S, I, G] =
        (s, pop, g) =>
          val breed = applyDynamicOperators[S, I, G](
            tournament(allRanks, tournamentRounds),
            genome andThen continuousValues,
            genome andThen discreteValues,
            continuousOperatorStatistics,
            discreteOperatorStatistics,
            discrete,
            operatorExploration,
            buildGenome)(s, pop, rng) //apply ()
          filterTooCloseFromArchiveOrPromising(breed)

      val offspring = breed(breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      val sizedOffspringGenomes = randomTake[G](offspring, lambda, rng)
      clonesReplace(cloneProbability, population, genome, tournament(ranks, tournamentRounds))(s, sizedOffspringGenomes, rng)


  def elitism[S, I: ClassTag, P, G](
                                     genome: I => G,
                                     history: I => Vector[P],
                                     aggregation: Vector[P] => Vector[Double],
                                     scaledValues: G => (IArray[Double], IArray[Int]),
                                     limit: Vector[Double],
                                     historySize: Int,
                                     mergeHistories: (Vector[I], Vector[I]) => Vector[I],
                                     mu: Int,
                                     archive: monocle.Lens[S, Archive[I]],
                                     distance: HDOSEOperation.TooClose,
                                     diversityDistance: monocle.Lens[S, Double],
                                     archiveSize: Int): Elitism[S, I] =
    (s1, population, candidates, rng) =>
      def fitness = NoisyOSEOperations.aggregated(history, aggregation)
      val memoizedFitness = mgo.tools.memoize(fitness)
      val merged = filterNaN(mergeHistories(population, candidates), memoizedFitness)

      // FIXME individuals can be close to each other but yet added to the archive
      def reachingIndividuals =
        merged.
          filter(i => history(i).size == historySize).
          filter(c => OSEOperation.patternIsReached(memoizedFitness(c), limit))

      val s2 = archive.modify(_ ++ reachingIndividuals)(s1)

      val s3 =
        if archive.get(s2).size <= archiveSize
        then s2
        else
          val newDiversityDistance =
            HDOSEOperation.computeDistance(
              distance,
              archive.get(s2),
              scaledValues,
              genome,
              archiveSize,
              diversityDistance.get(s2)
            )

          val newArchive =
            HDOSEOperation.shrinkArchive(
              distance,
              archive.get(s2),
              scaledValues,
              genome,
              newDiversityDistance
            )

          (archive.replace(newArchive) andThen diversityDistance.replace(newDiversityDistance))(s2)


      val filteredPopulation =
        merged.filterNot: i =>
          HDOSEOperation.isTooCloseFromArchive(
            distance,
            archive.get(s3),
            scaledValues,
            genome,
            diversityDistance.get(s3))(genome(i))

      NoisyNSGA2Operations.elitism[S, I, P](memoizedFitness, mergeHistories, mu)(s3, filteredPopulation, Vector.empty, rng)


