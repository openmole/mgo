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

import cats._
import cats.data._
import cats.implicits._

import freek._
import freedsl.dsl
import freedsl.random._
import freedsl.io._

import scala.language.higherKinds

object noisynsga2 {

  val context = dsl.merge(Random, StartTime, Generation, IO)
  import context._
  import context.implicits._

  def interpreter(s: EvolutionState[Unit]) =
    Random.interpreter(s.random) :&:
      StartTime.interpreter(s.startTime) :&:
      Generation.interpreter(s.generation) :&:
      IO.interpreter

  def oldest(population: Vector[Individual]) =
    if (population.isEmpty) population
    else {
      val maxHistory = population.map(_.fitnessHistory.size).max
      population.filter(_.fitnessHistory.size == maxHistory)
    }

  def aggregate(population: Vector[Individual], aggregation: Vector[Vector[Double]] => Vector[Double], scaling: Vector[Double] => Vector[Double]) =
    population.map(i => (scaling(i.genome.values.toVector), aggregation(vectorFitness.get(i))))

  def result(population: Vector[Individual], aggregation: Vector[Vector[Double]] => Vector[Double], scaling: Vector[Double] => Vector[Double]) =
    aggregate(oldest(population), aggregation, scaling)

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(genome: Genome, historyAge: Long, fitnessHistory: Array[Array[Double]], age: Long)

  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)
  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, 1, Array(f.toArray), 0)

  def vectorFitness = Individual.fitnessHistory composeLens array2ToVectorLens
  def vectorValues = Genome.values composeLens arrayToVectorLens

  def initialGenomes(lambda: Int, genomeSize: Int) =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(lambda, genomeSize)

  def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[M, Individual, Genome] =
    noisynsga2Operations.breeding[M, Individual, Genome](
      vectorFitness.get,
      aggregation,
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      buildGenome)(lambda, operatorExploration, cloneProbability)

  def expression(fitness: (util.Random, Vector[Double]) => Vector[Double]): Expression[(util.Random, Genome), Individual] =
    noisynsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism(mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double]): Elitism[M, Individual] =
    noisynsga2Operations.elitism[M, Individual](
      vectorFitness,
      aggregation,
      (Individual.genome composeLens vectorValues).get,
      Individual.age,
      Individual.historyAge
    )(mu, historySize)

  def state[M[_]: Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NoisyNSGA2 {

    implicit def isAlgorithm = new Algorithm[NoisyNSGA2, M, Individual, Genome, EvolutionState[Unit]] {
      def initialState(t: NoisyNSGA2, rng: util.Random) = EvolutionState[Unit](random = rng, s = ())

      def initialPopulation(t: NoisyNSGA2) =
        stochasticInitialPopulation[M, Genome, Individual](
          noisynsga2.initialGenomes(t.lambda, t.genomeSize),
          noisynsga2.expression(t.fitness))

      def step(t: NoisyNSGA2): Kleisli[M, Vector[Individual], Vector[Individual]] =
        noisynsga2Operations.step[M, Individual, Genome](
          noisynsga2.breeding(t.lambda, t.operatorExploration, t.cloneProbability, t.aggregation),
          noisynsga2.expression(t.fitness),
          noisynsga2.elitism(t.mu, t.historySize, t.aggregation))

      def state = noisynsga2.state[M]

      def run[A](m: M[A], s: EvolutionState[Unit]) = context.result(m, interpreter(s)).right.get
    }
  }

  case class NoisyNSGA2(
    mu: Int,
    lambda: Int,
    fitness: (util.Random, Vector[Double]) => Vector[Double],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genomeSize: Int,
    historySize: Int = 100,
    cloneProbability: Double = 0.2,
    operatorExploration: Double = 0.1)

  case class OpenMOLE(
    mu: Int,
    operatorExploration: Double,
    genomeSize: Int,
    historySize: Int,
    cloneProbability: Double,
    aggregation: Vector[Vector[Double]] => Vector[Double])

  object OpenMOLE {
    implicit def integration = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] with openmole.Stochastic {
      type M[A] = context.M[A]
      type G = Genome
      type I = Individual
      type S = EvolutionState[Unit]

      def iManifest = implicitly
      def gManifest = implicitly
      def sManifest = implicitly

      def mMonad = implicitly
      def mGeneration = implicitly
      def mStartTime = implicitly

      def operations(om: OpenMOLE) = new Ops {
        def randomLens = GenLens[S](_.random)
        def startTimeLens = GenLens[S](_.startTime)
        def generation(s: S) = s.generation
        def values(genome: G) = vectorValues.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = om.aggregation(vectorFitness.get(individual))
        def buildIndividual(genome: G, phenotype: Vector[Double]) = noisynsga2.buildIndividual(genome, phenotype)
        def initialState(rng: util.Random) = EvolutionState[Unit](random = rng, s = ())
        def initialGenomes(n: Int) = noisynsga2.initialGenomes(n, om.genomeSize)
        def breeding(n: Int) = noisynsga2.breeding(n, om.operatorExploration, om.cloneProbability, om.aggregation)
        def elitism = noisynsga2.elitism(om.mu, om.historySize, om.aggregation)

        def migrateToIsland(population: Vector[I]) = population.map(_.copy(historyAge = 0))
        def migrateFromIsland(population: Vector[I]) =
          population.filter(_.historyAge != 0).map {
            i => Individual.fitnessHistory.modify(_.take(scala.math.min(i.historyAge, om.historySize).toInt))(i)
          }
      }

      def run[A](x: M[A], s: S): (A, S) = {
        val res =
          for {
            xv <- x
            s <- noisynsga2.state[M]
          } yield (xv, s)
        context.result(res, interpreter(s)).right.get
      }

      def samples(i: I): Long = i.fitnessHistory.size
    }
  }

}

object noisynsga2Operations {

  def aggregated[I](fitness: I => Vector[Vector[Double]], aggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def breeding[M[_]: Monad: Random: Generation, I, G](
    history: I => Vector[Vector[Double]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      gs <- nsga2Operations.breeding[M, I, G](
        aggregated(history, aggregation),
        genome,
        genomeValues,
        genomeOperator,
        buildGenome
      )(lambda, operatorExploration) andThen clonesReplace[M, I, G](cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: Monad: Random: Generation, I](
    history: monocle.Lens[I, Vector[Vector[Double]]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long])(mu: Int, historySize: Int): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](historyAge, history)(historySize)) apply filterNaN(population, aggregated(history.get, aggregation))
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history.get, aggregation)) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def expression[G, I](
    values: G => Vector[Double],
    builder: (G, Vector[Double]) => I)(fitness: (util.Random, Vector[Double]) => Vector[Double]): Expression[(util.Random, G), I] = {
    case (rg, g) => builder(g, fitness(rg, values(g)))
  }

  def step[M[_]: Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(util.Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)
}