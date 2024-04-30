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
import mgo.tools.execution._

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

// TODO generify individual phenotype
object PSE {

  import CDGenome._

  type PSEState = EvolutionState[HitMap]

  case class Result(continuous: Vector[Double], discrete: Vector[Int], pattern: Vector[Int], phenotype: Vector[Double], individual: Individual)

  def result(population: Vector[Individual], continuous: Vector[C], pattern: Vector[Double] => Vector[Int]): Vector[Result] =
    population.map { i =>
      Result(
        scaleContinuousValues(continuousValues.get(i.genome), continuous),
        i.focus(_.genome) andThen discreteValues get,
        pattern(i.phenotype.toVector),
        i.phenotype.toVector,
        i)
    }

  def result(pse: PSE, population: Vector[Individual]): Vector[Result] =
    result(population, pse.continuous, pse.pattern)

  case class Individual(
    genome: Genome,
    phenotype: Array[Double])

  def buildIndividual(g: Genome, f: Vector[Double]): Individual = Individual(g, f.toArray)
  def vectorPhenotype: PLens[Individual, Individual, Vector[Double], Vector[Double]] = Focus[Individual](_.phenotype) andThen arrayToVectorIso[Double]

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding(
    lambda: Int,
    operatorExploration: Double,
    discrete: Vector[D],
    pattern: Vector[Double] => Vector[Int],
    reject: Option[Genome => Boolean]): Breeding[PSEState, Individual, Genome] =
    PSEOperations.adaptiveBreeding[PSEState, Individual, Genome](
      Focus[Individual](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      vectorPhenotype.get _ andThen pattern,
      buildGenome,
      lambda,
      reject,
      operatorExploration,
      Focus[PSEState](_.s))

  def elitism(pattern: Vector[Double] => Vector[Int], continuous: Vector[C]): Elitism[PSEState, Individual] =
    PSEOperations.elitism[PSEState, Individual, Vector[Double]](
      i => values(i.genome, continuous),
      vectorPhenotype.get,
      pattern,
      Focus[PSEState](_.s))

  def expression(phenotype: (Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): Genome => Individual =
    deterministic.expression[Genome, Vector[Double], Individual](
      values(_, continuous),
      buildIndividual,
      phenotype)

  def reject(pse: PSE): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous)

  implicit def isAlgorithm: Algorithm[PSE, Individual, Genome, EvolutionState[HitMap]] = new Algorithm[PSE, Individual, Genome, EvolutionState[HitMap]] {
    def initialState(t: PSE, rng: util.Random) = EvolutionState[HitMap](s = Map.empty)

    override def initialPopulation(t: PSE, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual](
        PSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        PSE.expression(t.phenotype, t.continuous),
        parallel)

    def step(t: PSE) =
      deterministic.step[EvolutionState[HitMap], Individual, Genome](
        PSE.adaptiveBreeding(t.lambda, t.operatorExploration, t.discrete, t.pattern, reject(t)),
        PSE.expression(t.phenotype, t.continuous),
        PSE.elitism(t.pattern, t.continuous),
        Focus[PSEState](_.generation),
        Focus[PSEState](_.evaluated))

  }

}

case class PSE(
  lambda: Int,
  phenotype: (Vector[Double], Vector[Int]) => Vector[Double],
  pattern: Vector[Double] => Vector[Int],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object PSEOperations {

  def adaptiveBreeding[S, I, G](
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    hitmap: monocle.Lens[S, HitMap]): Breeding[S, I, G] =
    (s, population, rng) => {
      val ranks = hitCountRanking(s, population, pattern, hitmap).map(x => -x)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      val breeding = applyDynamicOperators[S, I, G](
        tournament(ranks, logOfPopulationSize),
        genome andThen continuousValues,
        genome andThen discreteValues,
        continuousOperatorStatistics,
        discreteOperatorStatistics,
        discrete,
        operatorExploration,
        buildGenome)
      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population, rng)
      randomTake(offspring, lambda, rng)
    }

  def elitism[S, I, P: CanBeNaN](
    values: I => (Vector[Double], Vector[Int]),
    phenotype: I => P,
    pattern: P => Vector[Int],
    hitmap: monocle.Lens[S, HitMap]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedPattern = mgo.tools.memoize(phenotype andThen pattern)
      val noNan = filterNaN(candidates, phenotype)
      def keepFirst(i: Vector[I]) = Vector(i.head)
      val hm2 = addHits(memoizedPattern, noNan, hitmap.get(s))
      val elite = keepNiches(memoizedPattern, keepFirst)(population ++ noNan)
      (hitmap.set(hm2)(s), elite)


}
