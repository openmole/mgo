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
import tools._
import cats._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freek._
import freedsl.dsl
import freedsl.tool._
import freedsl.io._
import freedsl.random._

object nsga2 {

  val context = dsl.merge(Random, StartTime, Generation, IO)
  import context._
  import context.implicits._

  def interpreter(s: EvolutionState[Unit]) =
    Random.interpreter(s.random) :&:
      StartTime.interpreter(s.startTime) :&:
      Generation.interpreter(s.generation) :&:
      IO.interpreter

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(genome: Genome, fitness: Array[Double], age: Long)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)
  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)

  def vectorFitness = Individual.fitness composeLens arrayToVectorLens
  def vectorValues = Genome.values composeLens arrayToVectorLens

  def initialGenomes(lambda: Int, genomeSize: Int): M[Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(lambda, genomeSize)

  def breeding(lambda: Int, operatorExploration: Double): Breeding[M, Individual, Genome] =
    nsga2Operations.breeding[M, Individual, Genome](
      vectorFitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda, operatorExploration)

  def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    nsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism(mu: Int): Elitism[M, Individual] =
    nsga2Operations.elitism[M, Individual](
      vectorFitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(mu)

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness.toVector) }

  def state[M[_]: Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NSGA2 {

    implicit def isAlgorithm: Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] =
      new Algorithm[NSGA2, M, Individual, Genome, EvolutionState[Unit]] {
        override def initialState(t: NSGA2, rng: util.Random) = EvolutionState(random = rng, s = ())
        override def initialPopulation(t: NSGA2) =
          deterministicInitialPopulation[M, Genome, Individual](nsga2.initialGenomes(t.lambda, t.genomeSize), expression(t.fitness))
        override def step(t: NSGA2) =
          nsga2Operations.step(nsga2.breeding(t.lambda, t.operatorExploration), nsga2.expression(t.fitness), nsga2.elitism(t.mu))
        override def state = nsga2.state[M]
        override def run[A](m: M[A], s: EvolutionState[Unit]) = context.result(m, interpreter(s)).right.get
      }

  }

  case class NSGA2(mu: Int, lambda: Int, fitness: Vector[Double] => Vector[Double], genomeSize: Int, operatorExploration: Double = 0.1)

  case class OpenMOLE(mu: Int, genomeSize: Int, operatorExploration: Double)

  object OpenMOLE {

    implicit def integration: openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] {
      type M[T] = context.M[T]
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
        def generation(s: EvolutionState[Unit]) = s.generation
        def values(genome: G) = vectorValues.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = vectorFitness.get(individual)
        def buildIndividual(genome: G, phenotype: Vector[Double]) = nsga2.buildIndividual(genome, phenotype)
        def initialState(rng: util.Random) = EvolutionState[Unit](random = rng, s = ())
        def initialGenomes(n: Int) = nsga2.initialGenomes(n, om.genomeSize)
        def breeding(n: Int) = nsga2.breeding(n, om.operatorExploration)
        def elitism = nsga2.elitism(om.mu)
        def migrateToIsland(population: Vector[I]) = population
        def migrateFromIsland(population: Vector[I]) = population
      }

      def run[A](x: M[A], s: S): (A, S) = {
        val res =
          for {
            xv <- x
            s <- nsga2.state[M]
          } yield (xv, s)
        context.result(res, interpreter(s)).right.get
      }

    }

  }
}

object nsga2Operations {

  def breeding[M[_]: Monad: Generation: Random, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
      lambda: Int,
      operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperator[M, I](
        tournament[M, I, (Lazy[Int], Lazy[Double])](population, ranks),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration
      )
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case ((o1, o2), op) =>
          def gv1 = o1.map(math.clamp(_))
          def gv2 = o2.map(math.clamp(_))
          Vector(buildGenome(gv1, Some(op)), buildGenome(gv2, Some(op)))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
    } yield sizedOffspringGenomes
  }

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Vector[Double]) => I)(fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)))

  def elitism[M[_]: Monad: Random: Generation, I](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(mu: Int) = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, fitness)
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def step[M[_]: Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] =
    deterministicStep(breeding, expression, elitism)

}