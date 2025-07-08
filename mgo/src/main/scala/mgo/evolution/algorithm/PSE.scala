/*
 * Copyright (C) 16/12/2015 Guillaume Chérel
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

import scala.util.Random
import scala.language.higherKinds

// TODO generify individual phenotype
object PSE:

  import CDGenome._
  import CDGenome.DeterministicIndividual.Individual

  type PSEState = EvolutionState[HitMap]

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], pattern: Vector[Int], phenotype: P, individual: Individual[P])

  def result[P](population: Vector[Individual[P]], continuous: Vector[C], discrete: Vector[D], pattern: P => Vector[Int]): Vector[Result[P]] =
    population.map: i =>
      Result(
        scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous),
        i.focus(_.genome) andThen discreteVectorValues(discrete) get,
        pattern(i.phenotype),
        i.phenotype,
        i)

  def result[P](pse: PSE[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result(population, pse.continuous, pse.discrete, pse.pattern)

  def buildIndividual[P](g: Genome, f: P, generation: Long, initial: Boolean) = CDGenome.DeterministicIndividual.buildIndividual(g, f, generation, initial)
  //def vectorPhenotype[P]: PLens[Individual[P], Individual[P], Vector[Double], Vector[Double]] = Focus[Individual[P]](_.phenotype) andThen arrayToVectorIso[Double]

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P](
    lambda: Int,
    operatorExploration: Double,
    continuous: Vector[C],
    discrete: Vector[D],
    pattern: P => Vector[Int],
    maxRareSample: Int,
    reject: Option[Genome => Boolean]): Breeding[PSEState, Individual[P], Genome] =
    PSEOperations.adaptiveBreeding[PSEState, Individual[P], Genome](
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      Focus[Individual[P]](_.phenotype).get andThen pattern,
      buildGenome(discrete),
      lambda,
      reject,
      operatorExploration,
      Focus[PSEState](_.s),
      maxRareSample,
      (s, rng) => PSE.initialGenomes(s, continuous, discrete, reject, rng))

  def elitism[P: CanContainNaN](pattern: P => Vector[Int]): Elitism[PSEState, Individual[P]] =
    PSEOperations.elitism[PSEState, Individual[P], P](
      Focus[Individual[P]](_.phenotype).get,
      pattern,
      Focus[PSEState](_.s)
    )

  def expression[P](phenotype: (IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    deterministic.expression[Genome, P, Individual[P]](
      scaledValues(continuous, discrete),
      buildIndividual,
      phenotype)

  def reject[P](pse: PSE[P]): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous, pse.discrete)

  given isAlgorithm[P: CanContainNaN]: Algorithm[PSE[P], Individual[P], Genome, EvolutionState[HitMap]] with
    def initialState(t: PSE[P], rng: util.Random) = EvolutionState[HitMap](s = Map.empty)

    override def initialPopulation(t: PSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[P]](
        PSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        PSE.expression(t.phenotype, t.continuous, t.discrete),
        parallel)

    override def step(t: PSE[P]) =
      deterministic.step[EvolutionState[HitMap], Individual[P], Genome](
        PSE.adaptiveBreeding(t.lambda, t.operatorExploration, t.continuous, t.discrete, t.pattern, t.maxRareSample, reject(t)),
        PSE.expression(t.phenotype, t.continuous, t.discrete),
        PSE.elitism(t.pattern),
        Focus[PSEState](_.generation),
        Focus[PSEState](_.evaluated))


case class PSE[P](
  lambda: Int,
  phenotype: (IArray[Double], IArray[Int]) => P,
  pattern: P => Vector[Int],
  maxRareSample: Int = 10,
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object PSEOperations:

  def adaptiveBreeding[S, I, G](
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    pattern: I => Vector[Int],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    hitmap: monocle.Lens[S, HitMap],
    maxRareSample: Int,
    randomGenomes: (Int, Random) => Vector[G]): Breeding[S, I, G] =
    (s, population, rng) =>
      def allAtMaxSample =
        val hitMapValue = hitmap.get(s)
        population.forall(i => hitMapValue.getOrElse(pattern(i), 0) >= maxRareSample)

      if allAtMaxSample
      then randomGenomes(lambda, rng)
      else
        val ranks = hitCountRanking(s, population, pattern, hitmap).map(x => -x)
        val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
        val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
        val genomeValue = genome andThen (continuousValues, discreteValues).tupled

        val breeding = applyDynamicOperators[S, I, G](
          tournament(ranks, logOfPopulationSize),
          genomeValue,
          continuousOperatorStatistics,
          discreteOperatorStatistics,
          discrete,
          operatorExploration,
          buildGenome)
        breed(breeding, lambda, reject)(s, population, rng)

  def elitism[S, I, P: CanContainNaN](
    phenotype: I => P,
    pattern: P => Vector[Int],
    hitmap: monocle.Lens[S, HitMap]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedPattern = (phenotype andThen pattern).memoized
      val noNan = filterNaN(candidates, phenotype)
      def keepFirst(i: Vector[I]) = Vector(i.head)
      val hm2 = addHits(memoizedPattern, noNan, hitmap.get(s))
      val elite = keepNiches(memoizedPattern, keepFirst)(population ++ noNan)
      (hitmap.set(hm2)(s), elite)


