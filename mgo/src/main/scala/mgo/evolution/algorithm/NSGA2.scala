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

import monocle.macros.{ GenLens, Lenses }

import scala.language.higherKinds
import mgo.evolution._
import mgo.evolution.ranking._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.contexts._
import mgo.tools._
import mgo.tools.execution._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import shapeless._
import mgo.tagtools._

object NSGA2 {

  import CDGenome._
  import DeterministicIndividual._

  //  def breeding[M[_]: Generation: Random: cats.Monad](crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, Individual, Genome] =
  //    nsga2Operations.breeding[M, Individual, Genome](
  //      vectorFitness.get, Individual.genome.get, vectorValues.get, buildGenome(_, None))(crossover, mutation, lambda)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad, P](lambda: Int, operatorExploration: Double, discrete: Vector[D], fitness: P => Vector[Double]): Breeding[M, Individual[P], Genome] =
    NSGA2Operations.adaptiveBreeding[M, Individual[P], Genome](
      individualFitness[P](fitness),
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

  def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(express, components)

  def elitism[M[_]: cats.Monad: Random: Generation, P](mu: Int, components: Vector[C], fitness: P => Vector[Double]): Elitism[M, Individual[P]] =
    NSGA2Operations.elitism[M, Individual[P]](
      individualFitness[P](fitness),
      i => values(Individual.genome[P].get(i), components),
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result[P](population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double]) =
    keepFirstFront(population, individualFitness(fitness)).map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, individualFitness(fitness)(i))
    }

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.evolution.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2, M, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA2, M, Individual[Vector[Double]], Genome, EvolutionState[Unit]] {
      override def initialPopulation(t: NSGA2) =
        deterministic.initialPopulation[M, Genome, Individual[Vector[Double]]](
          NSGA2.initialGenomes[M](t.lambda, t.continuous, t.discrete),
          NSGA2.expression(t.fitness, t.continuous))
      override def step(t: NSGA2) =
        deterministic.step[M, Individual[Vector[Double]], Genome](
          NSGA2.adaptiveBreeding[M, Vector[Double]](t.lambda, t.operatorExploration, t.discrete, identity),
          NSGA2.expression(t.fitness, t.continuous),
          NSGA2.elitism[M, Vector[Double]](t.mu, t.continuous, identity))
      override def state = NSGA2.state[M]
    }

  def result(nsga2: NSGA2, population: Vector[Individual[Vector[Double]]]): Vector[Result] = result[Vector[Double]](population, nsga2.continuous, identity[Vector[Double]] _)

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
    mu: Int) = Elitism[M, I] { (population, candidates) =>
    val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), fitness)

    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  }

}
