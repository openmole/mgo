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
import mgo.evolution.niche._
import mgo.tools.*

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

object NoisyProfile:

  import CDGenome._
  import NoisyIndividual._

  type ProfileState = EvolutionState[Unit]

  def aggregatedFitness[N, P: Manifest](aggregation: Vector[P] => Vector[Double]): Individual[P] => Vector[Double] =
    NoisyNSGA2Operations.aggregated[Individual[P], P](vectorPhenotype[P].get, aggregation, _.phenotypeHistory.size)(_)

  case class Result[N, P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N, replications: Int, individual: Individual[P])

  def result[N, P: Manifest](
    population: Vector[Individual[P]],
    aggregation: Vector[P] => Vector[Double],
    niche: Individual[P] => N,
    continuous: Vector[C],
    discrete: Vector[D],
    onlyOldest: Boolean,
    keepAll: Boolean): Vector[Result[N, P]] =

    def nicheResult(population: Vector[Individual[P]]) =
      if onlyOldest
      then
        val front = keepFirstFront(population, aggregatedFitness(aggregation))
        front.maxByOption(_.phenotypeHistory.size).toVector
      else keepFirstFront(population, aggregatedFitness(aggregation))

    val individuals =
      if keepAll
      then population
      else nicheElitism(population, nicheResult, niche)

    individuals.map: i =>
      val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous, discrete)
      Result(c, d, f, niche(i), r, i)

  def result[N, P: Manifest](noisyProfile: NoisyProfile[N, P], population: Vector[Individual[P]], onlyOldest: Boolean = true): Vector[Result[N, P]] =
    result[N, P](population, noisyProfile.aggregation, noisyProfile.niche, noisyProfile.continuous, noisyProfile.discrete, onlyOldest, keepAll = false)

  def continuousProfile[P](continuous: Vector[C], x: Int, nX: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.continuousProfile[Individual[P]]((Focus[Individual[P]](_.genome) andThen continuousVectorValues(continuous)).get, x, nX)

  def discreteProfile[P](discrete: Vector[D], x: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.discreteProfile[Individual[P]]((Focus[Individual[P]](_.genome) andThen discreteVectorValues(discrete)).get, x)

  def boundedContinuousProfile[P](continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](i => scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile[P](continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](i => scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), x, intervals)

  def boundedObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, nX, min, max)

  def gridObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, intervals)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    continuous: Vector[C],
    discrete: Vector[D],
    reject: Option[Genome => Boolean]): Breeding[ProfileState, Individual[P], Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[ProfileState, Individual[P], Genome, P](
      aggregatedFitness(aggregation),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      buildGenome(discrete),
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      cloneProbability)

  def elitism[N, P: Manifest](niche: Niche[Individual[P], N], muByNiche: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], discrete: Vector[D]): Elitism[ProfileState, Individual[P]] =

    def individualValues(i: Individual[P]) = scaledValues(components, discrete)(i.genome)

    (s, population, candidates, rng) =>
      NoisyProfileOperations.elitism[ProfileState, Individual[P], N, P](
        aggregatedFitness(aggregation),
        mergeHistories(individualValues, vectorPhenotype, Focus[Individual[P]](_.historyAge), historySize),
        individualValues,
        niche,
        muByNiche)(s, population, candidates, rng)


  def expression[P: Manifest](fitness: (util.Random, IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    NoisyIndividual.expression[P](fitness, continuous, discrete)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def reject[N, P](pse: NoisyProfile[N, P]): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous, pse.discrete)

  given isAlgorithm[N, P: Manifest]: Algorithm[NoisyProfile[N, P], Individual[P], Genome, ProfileState] with
    override def initialState(t: NoisyProfile[N, P], rng: scala.util.Random) = EvolutionState(s = ())

    def initialPopulation(t: NoisyProfile[N, P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyProfile.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyProfile.expression[P](t.fitness, t.continuous, t.discrete),
        rng,
        parallel)

    def step(t: NoisyProfile[N, P]) =
      noisy.step[ProfileState, Individual[P], Genome](
        NoisyProfile.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.continuous,
          t.discrete,
          reject(t)),
        NoisyProfile.expression(t.fitness, t.continuous, t.discrete),
        NoisyProfile.elitism[N, P](
          t.niche,
          t.muByNiche,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.discrete),
        Focus[ProfileState](_.generation),
        Focus[ProfileState](_.evaluated))


case class NoisyProfile[N, P](
  muByNiche: Int,
  lambda: Int,
  fitness: (util.Random, IArray[Double], IArray[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  niche: Niche[CDGenome.NoisyIndividual.Individual[P], N],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object NoisyProfileOperations:

  def elitism[S, I, N, P](
    fitness: I => Vector[Double],
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    values: I => (IArray[Double], IArray[Int]),
    niche: Niche[I, N],
    muByNiche: Int): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedFitness = fitness.memoized
      def inNicheElitism(random: scala.util.Random)(p: Vector[I]) = keepOnFirstFront(p, memoizedFitness, muByNiche, random)

      val merged = mergeHistories(population, candidates)
      val filtered = filterNaN(merged, memoizedFitness)

      (s, nicheElitism[I, N](filtered, inNicheElitism(rng), niche))

