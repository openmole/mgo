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
package mgo.algorithm

import mgo.algorithm.GenomeVectorDouble._
import monocle.macros.{ Lenses, GenLens }

import mgo._
import ranking._
import tools._
import breeding._
import elitism._
import contexts._

import cats.data._
import cats.implicits._

import freedsl.dsl
import freedsl.tool._
import shapeless._

import scala.language.higherKinds

object NoisyNSGA2 {

  import CDGenome._
  import NoisyIndividual._

  def result(nsga2: NoisyNSGA2, population: Vector[Individual]) =
    keepFirstFront(population, NoisyNSGA2Operations.aggregated(vectorFitness.get, nsga2.aggregation)).map {
      i => NoisyIndividual.aggregate(i, nsga2.aggregation, nsga2.continuous)
    }

  //  def breeding[M[_]: cats.Monad: Random: Generation](crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, Individual, Genome] =
  //    noisynsga2Operations.breeding[M, Individual, Genome](
  //      vectorFitness.get,
  //      aggregation,
  //      Individual.genome.get,
  //      vectorValues.get,
  //      buildGenome(_, None))(crossover, mutation, lambda, cloneProbability)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation](lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double], discrete: Vector[D]): Breeding[M, Individual, Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[M, Individual, Genome](
      vectorFitness.get,
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

  def expression(fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    NoisyIndividual.expression(fitness, continuous)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], components: Vector[C]): Elitism[M, Individual] =
    NoisyNSGA2Operations.elitism[M, Individual](
      vectorFitness,
      aggregation,
      i => values(Individual.genome.get(i), components),
      Individual.age,
      Individual.historyAge,
      historySize,
      mu)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NoisyNSGA2, M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[NoisyNSGA2, M, Individual, Genome, EvolutionState[Unit]] {
    def initialPopulation(t: NoisyNSGA2) =
      noisy.initialPopulation[M, Genome, Individual](
        NoisyNSGA2.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyNSGA2.expression(t.fitness, t.continuous))

    def step(t: NoisyNSGA2): Kleisli[M, Vector[Individual], Vector[Individual]] =
      noisy.step[M, Individual, Genome](
        NoisyNSGA2.adaptiveBreeding[M](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete),
        NoisyNSGA2.expression(t.fitness, t.continuous),
        NoisyNSGA2.elitism[M](
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

case class NoisyNSGA2(
  mu: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double],
  aggregation: Vector[Vector[Double]] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyNSGA2Operations {

  def aggregated[I](fitness: I => Vector[Vector[Double]], aggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
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

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation, I, G](
    history: I => Vector[Vector[Double]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
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
      offspring <- breeding repeat ((lambda + 1) / 2)
      sizedOffspringGenomes <- randomTake[M, G](offspring.flatMap { case (g1, g2) => Vector(g1, g2) }, lambda)
      gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament(ranks, tournamentRounds)) apply sizedOffspringGenomes
    } yield gs
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    history: monocle.Lens[I, Vector[Vector[Double]]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long],
    historySize: Int,
    mu: Int): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](historyAge, history)(historySize)) apply filterNaN(population, aggregated(history.get, aggregation))
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history.get, aggregation)) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  } andThen incrementAge[M, I](age)

}