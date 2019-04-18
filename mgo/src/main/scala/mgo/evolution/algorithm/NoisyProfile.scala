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

import mgo.evolution._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.diversity._
import monocle.macros.{ GenLens, Lenses }
import mgo.evolution.niche._
import mgo.evolution.contexts._
import mgo.evolution.ranking._
import GenomeVectorDouble._
import cats.data._
import cats.implicits._
import shapeless._
import mgo.tagtools._
import mgo.evolution.niche
import mgo.tools._
import mgo.tools.execution._

import scala.language.higherKinds

object NoisyProfile {

  import CDGenome._
  import NoisyIndividual._

  def aggregatedFitness[N, P: Manifest](aggregation: Vector[P] => Vector[Double]) =
    NoisyNSGA2Operations.aggregated[Individual[P], P](vectorFitness[P].get, aggregation)(_)

  case class Result[N](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N, replications: Int)

  def result[N, P: Manifest](
    population: Vector[Individual[P]],
    aggregation: Vector[P] => Vector[Double],
    niche: Individual[P] => N,
    continuous: Vector[C],
    onlyOldest: Boolean) = {
    def nicheResult(population: Vector[Individual[P]]) =
      if (onlyOldest) population.sortBy(_.fitnessHistory.size).headOption.toVector
      else keepFirstFront(population, NoisyNSGA2Operations.aggregated(vectorFitness[P].get, aggregation))

    nicheElitism[Id, Individual[P], N](population, nicheResult, niche).map { i =>
      val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous)
      Result(c, d, f, niche(i), r)
    }
  }

  def result[N, P: Manifest](noisyProfile: NoisyProfile[N, P], population: Vector[Individual[P]], onlyOldest: Boolean = false): Vector[Result[N]] =
    result[N, P](population, noisyProfile.aggregation, noisyProfile.niche, noisyProfile.continuous, onlyOldest)

  def continuousProfile[P](x: Int, nX: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.continuousProfile[Individual[P]]((Individual.genome composeLens continuousValues).get _, x, nX)

  def discreteProfile[P](x: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.discreteProfile[Individual[P]]((Individual.genome composeLens discreteValues).get _, x)

  def boundedContinuousProfile[P](continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile[P](continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, intervals)

  def boundedObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, nX, min, max)

  def gridObjectiveProfile[P: Manifest](aggregation: Vector[P] => Vector[Double], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](aggregatedFitness(aggregation), x, intervals)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation, P: Manifest](lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[P] => Vector[Double], discrete: Vector[D]): Breeding[M, Individual[P], Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[M, Individual[P], Genome, P](
      vectorFitness[P].get,
      aggregation,
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      logOfPopulationSize,
      lambda,
      operatorExploration,
      cloneProbability)

  def elitism[M[_]: cats.Monad: Random: Generation, N, P: Manifest](niche: Niche[Individual[P], N], muByNiche: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C]): Elitism[M, Individual[P]] =
    NoisyProfileOperations.elitism[M, Individual[P], N, P](
      vectorFitness,
      aggregation,
      i => values(Individual.genome.get(i), components),
      Individual.historyAge,
      historySize,
      niche,
      muByNiche)

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](fitness, continuous)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.evolution.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, N, P: Manifest]: Algorithm[NoisyProfile[N, P], M, Individual[P], Genome, EvolutionState[Unit]] = new Algorithm[NoisyProfile[N, P], M, Individual[P], Genome, EvolutionState[Unit]] {
    def initialPopulation(t: NoisyProfile[N, P]) =
      noisy.initialPopulation[M, Genome, Individual[P]](
        NoisyProfile.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyProfile.expression[P](t.fitness, t.continuous))

    def step(t: NoisyProfile[N, P]) =
      noisy.step[M, Individual[P], Genome](
        NoisyProfile.adaptiveBreeding[M, P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete),
        NoisyProfile.expression(t.fitness, t.continuous),
        NoisyProfile.elitism[M, N, P](
          t.niche,
          t.muByNiche,
          t.historySize,
          t.aggregation,
          t.continuous))

    def state = NoisyProfile.state[M]
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
  operatorExploration: Double = 0.1)

object NoisyProfileOperations {

  def elitism[M[_]: cats.Monad: Random: Generation, I, N, P](
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    historyAge: monocle.Lens[I, Long],
    historySize: Int,
    niche: Niche[I, N],
    muByNiche: Int): Elitism[M, I] = Elitism[M, I] { (population, candidates) =>

    def agg = NoisyNSGA2Operations.aggregated(history.get, aggregation) _
    def inNicheElitism(p: Vector[I]) = keepFirstFront(p, agg).pure[M]

    val merged = mergeHistories(values, history, historyAge, historySize)(population, candidates)
    val filtered = filterNaN(merged, agg)

    nicheElitism(filtered, inNicheElitism, niche)
  }

}
