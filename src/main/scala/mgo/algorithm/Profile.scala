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
import contexts._
import mgo.niche._
import mgo.breeding._
import mgo.elitism._
import mgo.ranking._
import mgo.tools._
import GenomeVectorDouble._
import cats.data._
import cats.implicits._
import freedsl.tool._
import mgo.niche
import shapeless._

object Profile extends niche.Imports {

  import CDGenome._
  import DeterministicIndividual._

  case class Result[N](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N)

  def result[N](population: Vector[Individual], niche: Individual => N, continuous: Vector[C]) =
    nicheElitism[Id, Individual, N](population, keepFirstFront(_, vectorFitness.get), niche).map { i =>
      Result(
        scaleContinuousValues(continuousValues.get(i.genome), continuous),
        Individual.genome composeLens discreteValues get i,
        i.fitness.toVector,
        niche(i))
    }

  def result[N](profile: Profile[N], population: Vector[Individual]): Vector[Result[N]] =
    result(population, profile.niche, profile.continuous)

  def continuousProfile(x: Int, nX: Int): Niche[Individual, Int] =
    continuousProfile[Individual]((Individual.genome composeLens continuousValues).get _, x, nX)

  def discreteProfile(x: Int): Niche[Individual, Int] =
    discreteProfile[Individual]((Individual.genome composeLens discreteValues).get _, x)

  def boundedContinuousProfile(continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual, Int] =
    boundedContinuousProfile[Individual](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile(continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual, Int] =
    gridContinuousProfile[Individual](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, intervals)

  def boundedObjectiveProfile(x: Int, nX: Int, min: Double, max: Double): Niche[Individual, Int] =
    boundedContinuousProfile[Individual](vectorFitness.get _, x, nX, min, max)

  def gridObjectiveProfile(x: Int, intervals: Vector[Double]): Niche[Individual, Int] =
    gridContinuousProfile[Individual](vectorFitness.get _, x, intervals)

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
      logOfPopulationSize,
      lambda,
      operatorExploration)

  def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C]): Genome => Individual =
    DeterministicIndividual.expression(fitness, components)

  def elitism[M[_]: cats.Monad: Random: Generation, N](niche: Niche[Individual, N], mu: Int, components: Vector[C]): Elitism[M, Individual] =
    ProfileOperations.elitism[M, Individual, N](
      vectorFitness.get,
      i => values(Individual.genome.get(i), components),
      niche,
      mu)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  implicit def isAlgorithm[M[_]: cats.Monad: Generation: Random: StartTime, N]: Algorithm[Profile[N], M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[Profile[N], M, Individual, Genome, EvolutionState[Unit]] {
    def initialPopulation(t: Profile[N]) =
      deterministic.initialPopulation[M, Genome, Individual](
        Profile.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        Profile.expression(t.fitness, t.continuous))

    def step(t: Profile[N]) =
      deterministic.step[M, Individual, Genome](
        Profile.adaptiveBreeding(t.lambda, t.operatorExploration, t.discrete),
        Profile.expression(t.fitness, t.continuous),
        Profile.elitism(t.niche, t.nicheSize, t.continuous))

    def state = Profile.state[M]
  }

}

case class Profile[N](
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  niche: Niche[CDGenome.DeterministicIndividual.Individual, N],
  nicheSize: Int = 20,
  operatorExploration: Double = 0.1)

object ProfileOperations {

  import scala.math._

  //  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
  //    fitness: I => Vector[Double],
  //    genome: I => G,
  //    continuousValues: G => Vector[Double],
  //    continuousOperator: G => Option[Int],
  //    discreteValues: G => Vector[Int],
  //    discreteOperator: G => Option[Int],
  //    discrete: Vector[D],
  //    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
  //    lambda: Int,
  //    niche: Niche[I, Int],
  //    operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>
  //    for {
  //      //ranks <- profileRanking[M, I](niche, fitness) apply population
  //      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
  //      continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
  //      discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
  //      breeding = applyDynamicOperators[M, I, G](
  //        tournament(ranks, rounds = size => round(log10(size).toInt)),
  //        genome andThen continuousValues,
  //        genome andThen discreteValues,
  //        continuousOperatorStatistics,
  //        discreteOperatorStatistics,
  //        discrete,
  //        operatorExploration,
  //        buildGenome) apply population
  //      offspring <- breeding repeat ((lambda + 1) / 2)
  //      sizedOffspringGenomes <- randomTake[M, G](offspring.flatMap { case (g1, g2) => List(g1, g2) }, lambda)
  //    } yield sizedOffspringGenomes
  //  }

  //  def expression[G, I](
  //    values: G => (Vector[Double], Vector[Int]),
  //    build: (G, Double) => I)(fitness: (Vector[Double], Vector[Int]) => Vector[Double]): G => I = {
  //    (g: G) =>
  //      val (cs, ds) = values(g)
  //      build(g, fitness(cs, ds))
  //  }

  //  def elitism[M[_]: cats.Monad: Random: Generation, I, N](
  //    fitness: I => Vector[Double],
  //    values: I => Vector[Double],
  //    age: monocle.Lens[I, Long])(niche: Niche[I, N]): Elitism[M, I] = Elitism[M, I] { population =>
  //    for {
  //      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, fitness)
  //      elite <- keepNiches[M, I, N](niche, minimiseO[M, I, Double](fitness, 1)) apply cloneRemoved
  //    } yield elite
  //  } andThen incrementGeneration[M, I](age)

  def elitism[M[_]: cats.Monad: Random: Generation, I, N](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    niche: Niche[I, N],
    muByNiche: Int) = {
    def nsga2Elitism(population: Vector[I]) = NSGA2Operations.elitism[M, I](fitness, values, muByNiche).apply(population)
    Elitism[M, I] { nicheElitism(_, nsga2Elitism, niche) }
  }

}
