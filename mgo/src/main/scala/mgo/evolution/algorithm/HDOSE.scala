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

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object HDOSE:

  import CDGenome._
  import DeterministicIndividual._
  import monocle._

  case class StateType[P](archive: Archive[Individual[P]], distance: Double)
  type HDOSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P]: Lens[HDOSEState[P], Archive[Individual[P]]] = Focus[HDOSEState[P]](_.s.archive)
  def distanceLens[P]: Lens[HDOSEState[P], Double] = Focus[HDOSEState[P]](_.s.distance)

  def initialState[P](distance: Double = 1.0): HDOSEState[P] = EvolutionState(s = StateType(Archive.empty, distance))

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P](
    lambda: Int,
    operatorExploration: Double,
    continuous: Vector[C],
    discrete: Vector[D],
    weightC: Vector[Double],
    weightD: Vector[Double],
    fitness: P => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[HDOSEState[P], Individual[P], Genome] =
    HDOSEOperation.adaptiveBreeding[HDOSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      tooCloseByComponent(weightC, weightD, discrete),
      distanceLens.get,
      buildGenome(discrete),
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      archiveLens[P].get)

  def expression[P](fitness: (IArray[Double], IArray[Int]) => P, components: Vector[C], discrete: Vector[D]): (Genome, Long, Boolean) => Individual[P] =
    DeterministicIndividual.expression(fitness, components, discrete)

  def elitism[P](
    mu: Int,
    limit: Vector[Double],
    weightC: Vector[Double],
    weightD: Vector[Double],
    archiveSize: Int,
    continuous: Vector[C],
    discrete: Vector[D],
    fitness: P => Vector[Double],
    precision: Double): Elitism[HDOSEState[P], Individual[P]] =
    HDOSEOperation.elitism[HDOSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      limit,
      scaledValues(continuous, discrete),
      mu,
      archiveLens[P],
      tooCloseByComponent(weightC, weightD, discrete),
      precision,
      distanceLens,
      archiveSize,
      continuousValues(continuous).get,
      discreteValues(discrete).get,
      Focus[Individual[P]](_.genome).get
    )

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P], archive: Boolean)

  def result[P](state: HDOSEState[P], population: Vector[Individual[P]], continuous: Vector[C], discrete: Vector[D], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] =
    def individualToResult(i: Individual[P], archive: Boolean) =
      Result(scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), i.focus(_.genome) andThen discreteVectorValues(discrete) get, DeterministicIndividual.individualFitness(fitness)(i), i, archive)

    val goodIndividuals =
      if keepAll
      then population.map(i => individualToResult(i, false))
      else Seq()

    val archiveIndividuals =
      archiveLens.get(state).toVector.map(i => individualToResult(i, true))

    archiveIndividuals ++ goodIndividuals

  def result(t: HDOSE, state: HDOSEState[Vector[Double]], population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] =
    result[Vector[Double]](state = state, continuous = t.continuous, discrete = t.discrete, fitness = identity, population = population, keepAll = false)

  def reject(t: HDOSE): Option[Genome => Boolean] = NSGA2.reject(t.reject, t.continuous, t.discrete)

  def tooCloseByComponent(weightC: Vector[Double], weightD: Vector[Double], discrete: Vector[D]): HDOSEOperation.TooClose = (g1, g2, d) =>
    val (c1, d1) = g1
    val (c2, d2) = g2

    var sum = 0.0

    var ic = 0
    val cSize = c1.size

    while ic < cSize && sum < d
    do
      sum += Math.abs(c1(ic) - c2(ic)) * weightC(ic)
      ic += 1


    var id = 0
    val dSize = d1.size

    while id < dSize && sum < d
    do
      val d = discrete(id)
      def scale(x: Int) =
        changeScale(x, d.low, d.high, 0.0, 1.0)

      sum += Math.abs(scale(d1(id)) - scale(d2(id))) * weightD(id)
      id += 1

    sum < d

  given Algorithm[HDOSE, Individual[Vector[Double]], Genome, HDOSEState[Vector[Double]]] with
    override def initialState(t: HDOSE, rng: scala.util.Random) = HDOSE.initialState(t.distance)
    override def initialPopulation(t: HDOSE, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        HDOSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        HDOSE.expression(t.fitness, t.continuous, t.discrete),
        parallel)

    def step(t: HDOSE) =
      val sC = t.weightC.getOrElse(Vector.fill(t.continuous.size)(1.0))
      val sD = t.weightD.getOrElse(Vector.fill(t.discrete.size)(1.0))

      deterministic.step[HDOSEState[Vector[Double]], Individual[Vector[Double]], Genome](
        HDOSE.adaptiveBreeding[Vector[Double]](t.lambda, t.operatorExploration, t.continuous, t.discrete, sC, sD, identity, reject(t)),
        HDOSE.expression(t.fitness, t.continuous, t.discrete),
        HDOSE.elitism(t.mu, t.limit, sC, sD, t.archiveSize, t.continuous, t.discrete, identity, t.distance),
        Focus[HDOSEState[Vector[Double]]](_.generation),
        Focus[HDOSEState[Vector[Double]]](_.evaluated))


case class HDOSE(
  mu: Int,
  lambda: Int,
  fitness: (IArray[Double], IArray[Int]) => Vector[Double],
  limit: Vector[Double],
  archiveSize: Int = 1000,
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  weightC: Option[Vector[Double]] = None,
  weightD: Option[Vector[Double]] = None,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None,
  distance: Double = 1.0)

object HDOSEOperation:

  type GenomeValue =  (IArray[Double], IArray[Int])
  type TooClose = (GenomeValue, GenomeValue, Double) => Boolean

  def isTooCloseFromArchive[G, I](
    tooClose: TooClose,
    archive: Archive[I],
    continuousValues: G => IArray[Double],
    discreteValues: G => IArray[Int],
    genome: I => G,
    diversityDistance: Double)(g: G): Boolean =

    def tooCloseGenome(g1: G, g2: G, d: Double): Boolean =
      def genomeValues(g: G) = (continuousValues(g), discreteValues(g))
      tooClose(genomeValues(g1), genomeValues(g2), d)

    def tooCloseFromArchive(g: G) =
      archive.exists(i => tooCloseGenome(genome(i), g, diversityDistance))

    tooCloseFromArchive(g)

  def shrinkArchive[G, I: ClassTag](
    distance: TooClose,
    archive: Archive[I],
    continuousValues: G => IArray[Double],
    discreteValues: G => IArray[Int],
    genome: I => G,
    diversityDistance: Double): Archive[I] =

    def isTooClose(archive: Archive[I], g: G) =
      isTooCloseFromArchive(distance, archive, continuousValues, discreteValues, genome, diversityDistance)(g)

    val newArchive = ListBuffer[I]()
    newArchive.addOne(archive.head)

    for
      i <- archive.tail
      if !isTooClose(IArray.unsafeFromArray(newArchive.toArray), genome(i))
    do newArchive.addOne(i)

    IArray.unsafeFromArray(newArchive.toArray)

  def computeDistance[G, I: ClassTag](
    distance: TooClose,
    archive: Archive[I],
    continuousValues: G => IArray[Double],
    discreteValues: G => IArray[Int],
    genome: I => G,
    targetSize: Int,
    currentDistance: Double,
    precision: Double) =
    def computeSize(d: Double) =
      shrinkArchive(distance, archive, continuousValues, discreteValues, genome, d).size.toDouble

    val newDistance =
      mgo.tools.findFirstUnder(
        targetSize,
        computeSize,
        currentDistance,
        precision
      )

    newDistance


  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    distance: TooClose,
    diversityDistance: S => Double,
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
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
              continuousValues,
              discreteValues,
              genome,
              diversityDistance(s))

      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      randomTake(offspring, lambda, rng)


  def elitism[S, I: ClassTag, G](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    scaledValues: G => (IArray[Double], IArray[Int]),
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    distance: TooClose,
    precision: Double,
    diversityDistance: Lens[S, Double],
    archiveSize: Int,
    continuousValues: G => IArray[Double],
    discreteValues: G => IArray[Int],
    genome: I => G): Elitism[S, I] =
    (s1, population, candidates, rng) =>
      val memoizedFitness = mgo.tools.memoize(fitness)
      val cloneRemoved = filterNaN(keepFirst(genome andThen scaledValues)(population, candidates), memoizedFitness)

      // FIXME individuals can be close to each other but yet added to the archive
      def newlyReaching = candidates.filter(c => OSEOperation.patternIsReached(memoizedFitness(c), limit))

      val s2 = archive.modify(_ ++ newlyReaching)(s1)

      val s3 =
        if archive.get(s2).size <= archiveSize
        then s2
        else
          val newDiversityDistance =
            computeDistance(
              distance,
              archive.get(s2),
              continuousValues,
              discreteValues,
              genome,
              archiveSize,
              diversityDistance.get(s2),
              precision
            )

          val newArchive =
            shrinkArchive(
              distance,
              archive.get(s2),
              continuousValues,
              discreteValues,
              genome,
              newDiversityDistance
            )

          (archive.replace(newArchive) andThen diversityDistance.replace(newDiversityDistance))(s2)

      val filteredPopulation =
        cloneRemoved.filterNot: i =>
          isTooCloseFromArchive(
            distance,
            archive.get(s3),
            continuousValues,
            discreteValues,
            genome,
            diversityDistance.get(s3))(genome(i))

      NSGA2Operations.elitism(memoizedFitness, genome andThen scaledValues, mu)(s3, filteredPopulation, Vector.empty, rng)



