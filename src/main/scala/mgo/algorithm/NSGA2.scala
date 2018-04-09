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

import monocle.macros.{ GenLens, Lenses }

import scala.language.higherKinds
import mgo._
import mgo.ranking._
import mgo.breeding._
import mgo.elitism._
import mgo.contexts._
import mgo.tools._
import tools._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freedsl.tool._
import shapeless._

object NSGA2 {

  import CDGenome._
  import DeterministicIndividual._

  //  def breeding[M[_]: Generation: Random: cats.Monad](crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, Individual, Genome] =
  //    nsga2Operations.breeding[M, Individual, Genome](
  //      vectorFitness.get, Individual.genome.get, vectorValues.get, buildGenome(_, None))(crossover, mutation, lambda)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad](lambda: Int, operatorExploration: Double, discrete: Vector[D]): Breeding[M, Individual, Genome] =
    NSGA2Operations.adaptiveBreeding[M, Individual, Genome](
      vectorFitness.get,
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      _ => 1,
      lambda,
      operatorExploration)

  def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C]): Genome => Individual =
    DeterministicIndividual.expression(fitness, components)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int, components: Vector[C]): Elitism[M, Individual] =
    NSGA2Operations.elitism[M, Individual](
      vectorFitness.get,
      i => values(Individual.genome.get(i), components),
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result(population: Vector[Individual], continuous: Vector[C]) =
    keepFirstFront(population, vectorFitness.get).map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
    }

  def result(nsga2: NSGA2, population: Vector[Individual]): Vector[Result] = result(population, nsga2.continuous)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA2) =
        deterministic.initialPopulation[M, Genome, Individual](
          NSGA2.initialGenomes[M](t.lambda, t.continuous, t.discrete),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2) =
        deterministic.step[M, Individual, Genome](
          NSGA2.adaptiveBreeding[M](t.lambda, t.operatorExploration, t.discrete),
          NSGA2.expression(t.fitness, t.continuous),
          NSGA2.elitism(t.mu, t.continuous))
      override def state = NSGA2.state[M]
    }

}

case class NSGA2(
  mu: Int,
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1)

object NSGA2Operations {

  //  def breeding[M[_]: cats.Monad: Generation: Random, I, G](
  //    fitness: I => Vector[Double],
  //    genome: I => G,
  //    genomeValues: G => Vector[Double],
  //    buildGenome: (Vector[Double], Vector[Long]) => G)(crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, I, G] = Breeding { population =>
  //    for {
  //      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
  //      breeding = applyOperators[M, I, Vector[Double]](crossover, mutation, tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), genome andThen genomeValues) apply population
  //      offspring <- breeding repeat ((lambda + 1) / 2)
  //      offspringGenomes = offspring.flatMap {
  //        case (o1, o2) =>
  //          def gv1 = o1.map(math.clamp(_))
  //          def gv2 = o2.map(math.clamp(_))
  //          Vector(buildGenome(gv1), buildGenome(gv2))
  //      }
  //      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
  //    } yield sizedOffspringGenomes
  //  }

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- ranking.paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
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
      offspring <- breeding.accumulate(lambda)
      sizedOffspringGenomes <- randomTake[M, G](offspring, lambda)
    } yield sizedOffspringGenomes
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    mu: Int) = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepFirst[M, I]) apply filterNaN(population, fitness)
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }

}
