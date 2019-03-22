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

import mgo.evolution.algorithm.GenomeVectorDouble._
import monocle.macros.{ Lenses, GenLens }

import mgo.evolution._
import mgo.evolution.ranking._
import mgo.tools._
import mgo.tools.execution._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.contexts._

import cats.data._
import cats.implicits._

import mgo.tagtools._
import shapeless._

import scala.language.higherKinds

object NoisyNSGA2 {

  import CDGenome._
  import NoisyIndividual._

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result[P: Manifest](population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C]) =
    keepFirstFront(population, NoisyNSGA2Operations.aggregated(vectorFitness[P].get, aggregation)).map {
      i =>
        val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
        Result(c, d, f, r)
    }

  def result[P: Manifest](nsga2: NoisyNSGA2[P], population: Vector[Individual[P]]): Vector[Result] =
    result[P](population, nsga2.aggregation, nsga2.continuous)

  //  def breeding[M[_]: cats.Monad: Random: Generation](crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, Individual, Genome] =
  //    noisynsga2Operations.breeding[M, Individual, Genome](
  //      vectorFitness.get,
  //      aggregation,
  //      Individual.genome.get,
  //      vectorValues.get,
  //      buildGenome(_, None))(crossover, mutation, lambda, cloneProbability)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

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
      _ => 1,
      lambda,
      operatorExploration,
      cloneProbability)

  def expression[P: Manifest](phenotype: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](phenotype, continuous)

  def elitism[M[_]: cats.Monad: Random: Generation, P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C]): Elitism[M, Individual[P]] = {
    def individualValues(i: Individual[P]) = values(Individual.genome.get(i), components)

    NoisyNSGA2Operations.elitism[M, Individual[P], P](
      vectorFitness[P].get,
      aggregation,
      individualValues,
      mergeHistories(individualValues, vectorFitness[P], Individual.historyAge[P], historySize),
      mu)
  }

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.evolution.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, P: Manifest]: Algorithm[NoisyNSGA2[P], M, Individual[P], Genome, EvolutionState[Unit]] = new Algorithm[NoisyNSGA2[P], M, Individual[P], Genome, EvolutionState[Unit]] {
    def initialPopulation(t: NoisyNSGA2[P]) =
      noisy.initialPopulation[M, Genome, Individual[P]](
        NoisyNSGA2.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyNSGA2.expression[P](t.fitness, t.continuous))

    def step(t: NoisyNSGA2[P]): Kleisli[M, Vector[Individual[P]], Vector[Individual[P]]] =
      noisy.step[M, Individual[P], Genome](
        NoisyNSGA2.adaptiveBreeding[M, P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete),
        NoisyNSGA2.expression(t.fitness, t.continuous),
        NoisyNSGA2.elitism[M, P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous))
    //          case ManualOperators(crossover, mutation) =>
    //            noisynsga2Operations.step[M, Individual, Genome](
    //              noisynsga2.breeding[M](crossover, mutation, t.lambda, t.cloneProbability, t.aggregation),
    //              noisynsga2.expression(t.fitness),
    //              noisynsga2.elitism[M](t.mu, t.historySize, t.aggregation))

    def state = NoisyNSGA2.state[M]
  }

}

case class NoisyNSGA2[P](
  mu: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyNSGA2Operations {

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  //  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
  //    history: I => Vector[Vector[Double]],
  //    aggregation: Vector[Vector[Double]] => Vector[Double],
  //    genome: I => G,
  //    genomeValues: G => Vector[Double],
  //    buildGenome: Vector[Double] => G)(crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int, cloneProbability: Double): Breeding[M, I, G] =
  //    for {
  //      population <- Kleisli.ask[M, Vector[I]]
  //      gs <- nsga2Operations.breeding[M, I, G](
  //        aggregated(history, aggregation),
  //        genome,
  //        genomeValues,
  //        buildGenome)(crossover, mutation, lambda) andThen clonesReplace[M, I, G](cloneProbability, population, genome)
  //    } yield gs

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation, I, G, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- ranking.paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history, aggregation)) apply population
      continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      breeding = applyDynamicOperators[M, I, G](
        tournament(ranks, tournamentRounds),
        genome andThen continuousValues,
        genome andThen discreteValues,
        continuousOperatorStatistics,
        discreteOperatorStatistics,
        discrete,
        operatorExploration,
        buildGenome) apply population
      offspring <- breeding accumulate lambda
      sizedOffspringGenomes <- randomTake[M, G](offspring, lambda)
      gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament(ranks, tournamentRounds)) apply sizedOffspringGenomes
    } yield gs
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int): Elitism[M, I] = Elitism[M, I] { (population, candidates) =>
    val merged = filterNaN(mergeHistories(population, candidates), aggregated(history, aggregation))

    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history, aggregation)) apply merged
      elite = keepHighestRanked(merged, ranks, mu)
    } yield elite
  }

}
