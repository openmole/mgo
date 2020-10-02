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
import mgo.tools.execution._

import scala.language.higherKinds

object NoisyProfile {

  import CDGenome._
  import NoisyIndividual._

  type ProfileState = EvolutionState[Unit]

  def aggregatedFitness[N, P: Manifest](aggregation: Vector[P] => Vector[Double]) =
    NoisyNSGA2Operations.aggregated[Individual[P], P](vectorPhenotype[P].get, aggregation, Individual.phenotypeHistory[P].get(_).size)(_)

  case class Result[N, P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N, replications: Int, individual: Individual[P])

  def result[N, P: Manifest](
    population: Vector[Individual[P]],
    aggregation: Vector[P] => Vector[Double],
    niche: Individual[P] => N,
    continuous: Vector[C],
    onlyOldest: Boolean,
    keepAll: Boolean) = {

    def nicheResult(population: Vector[Individual[P]]) = {
      if (onlyOldest) {
        val front = keepFirstFront(population, aggregatedFitness(aggregation))
        front.sortBy(-_.phenotypeHistory.size).headOption.toVector
      } else keepFirstFront(population, aggregatedFitness(aggregation))
    }

    val individuals = if (keepAll) population else nicheElitism[Individual[P], N](population, nicheResult, niche)

    individuals.map { i =>
      val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous)
      Result(c, d, f, niche(i), r, i)
    }
  }

  def result[N, P: Manifest](noisyProfile: NoisyProfile[N, P], population: Vector[Individual[P]], onlyOldest: Boolean = true): Vector[Result[N, P]] =
    result[N, P](population, noisyProfile.aggregation, noisyProfile.niche, noisyProfile.continuous, onlyOldest, keepAll = false)

  def continuousProfile[P](x: Int, nX: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.continuousProfile[Individual[P]]((Individual.genome[P] composeLens continuousValues).get _, x, nX)

  def discreteProfile[P](x: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.discreteProfile[Individual[P]]((Individual.genome[P] composeLens discreteValues).get _, x)

  def boundedContinuousProfile[P](continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile[P](continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, intervals)

  def boundedObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, nX, min, max)

  def gridObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, intervals)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    discrete: Vector[D],
    reject: Option[Genome => Boolean]) =
    NoisyNSGA2Operations.adaptiveBreeding[ProfileState, Individual[P], Genome, P](
      aggregatedFitness(aggregation),
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      cloneProbability)

  def elitism[N, P: Manifest](niche: Niche[Individual[P], N], muByNiche: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C]) = {

    def individualValues(i: Individual[P]) = values(Individual.genome.get(i), components)

    NoisyProfileOperations.elitism[ProfileState, Individual[P], N, P](
      aggregatedFitness(aggregation),
      mergeHistories(individualValues, vectorPhenotype, Individual.historyAge, historySize),
      individualValues,
      niche,
      muByNiche)
  }

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](fitness, continuous)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random) =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def reject[N, P](pse: NoisyProfile[N, P]) = NSGA2.reject(pse.reject, pse.continuous)

  implicit def isAlgorithm[N, P: Manifest]: Algorithm[NoisyProfile[N, P], Individual[P], Genome, ProfileState] = new Algorithm[NoisyProfile[N, P], Individual[P], Genome, ProfileState] {
    override def initialState(t: NoisyProfile[N, P], rng: scala.util.Random) = EvolutionState(s = ())

    def initialPopulation(t: NoisyProfile[N, P], rng: scala.util.Random) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyProfile.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyProfile.expression[P](t.fitness, t.continuous),
        rng)

    def step(t: NoisyProfile[N, P]) =
      noisy.step[ProfileState, Individual[P], Genome](
        NoisyProfile.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          reject(t)),
        NoisyProfile.expression(t.fitness, t.continuous),
        NoisyProfile.elitism[N, P](
          t.niche,
          t.muByNiche,
          t.historySize,
          t.aggregation,
          t.continuous),
        EvolutionState.generation)

  }

}

case class NoisyProfile[N, P](
  muByNiche: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  niche: Niche[CDGenome.NoisyIndividual.Individual[P], N],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object NoisyProfileOperations {

  def elitism[S, I, N, P](
    fitness: I => Vector[Double],
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    values: I => (Vector[Double], Vector[Int]),
    niche: Niche[I, N],
    muByNiche: Int): Elitism[S, I] =
    (s, population, candidates, rng) => {

      def inNicheElitism(random: scala.util.Random)(p: Vector[I]) = keepOnFirstFront(p, fitness, muByNiche, random)

      val merged = mergeHistories(population, candidates)
      val filtered = filterNaN(merged, fitness)

      (s, nicheElitism[I, N](filtered, inNicheElitism(rng), niche))
    }

}
