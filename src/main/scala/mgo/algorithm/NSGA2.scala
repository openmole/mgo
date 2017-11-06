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
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freedsl.tool._

object nsga2 {

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])

  @Lenses case class Individual(genome: Genome, fitness: Array[Double], age: Long)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)

  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)

  def vectorFitness = Individual.fitness composeLens arrayToVectorLens

  def vectorValues = Genome.values composeLens arrayToVectorLens

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, genomeSize: Int): M[Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(lambda, genomeSize)

  def breeding[M[_]: Generation: Random: cats.Monad](crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, Individual, Genome] =
    nsga2Operations.breeding[M, Individual, Genome](
      vectorFitness.get, Individual.genome.get, vectorValues.get, buildGenome(_, None)
    )(crossover, mutation, lambda)

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad](lambda: Int, operatorExploration: Double): Breeding[M, Individual, Genome] =
    nsga2Operations.adaptiveBreeding[M, Individual, Genome](
      vectorFitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda, operatorExploration)

  def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    nsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism[M[_]: cats.Monad: Random: Generation](mu: Int): Elitism[M, Individual] =
    nsga2Operations.elitism[M, Individual](
      vectorFitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(mu)

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness.toVector) }

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NSGA2 {

    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

    implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NSGA2[M], M, Individual, Genome, EvolutionState[Unit]] =
      new Algorithm[NSGA2[M], M, Individual, Genome, EvolutionState[Unit]] {
        override def initialPopulation(t: NSGA2[M]) =
          deterministicInitialPopulation[M, Genome, Individual](nsga2.initialGenomes[M](t.lambda, t.genomeSize), expression(t.fitness))
        override def step(t: NSGA2[M]) =
          t.operators match {
            case AdaptiveOperators(operatorExploration) => nsga2Operations.step[M, Individual, Genome](nsga2.adaptiveBreeding[M](t.lambda, operatorExploration), nsga2.expression(t.fitness), nsga2.elitism(t.mu))
            case ManualOperators(crossover, mutation) => nsga2Operations.step[M, Individual, Genome](nsga2.breeding[M](crossover, mutation, t.lambda), nsga2.expression(t.fitness), nsga2.elitism(t.mu))
          }
        override def state = nsga2.state[M]
      }
  }

  case class NSGA2[M[_]](
    mu: Int,
    lambda: Int,
    fitness: Vector[Double] => Vector[Double],
    genomeSize: Int,
    operators: Operators[M] = AdaptiveOperators[M](0.1))

}

object nsga2Operations {

  def breeding[M[_]: cats.Monad: Generation: Random, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    buildGenome: Vector[Double] => G)(crossover: GACrossover[M], mutation: GAMutation[M], lambda: Int): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
      breeding = applyOperators[M, I, Vector[Double]](crossover, mutation, tournament[M, I, (Lazy[Int], Lazy[Double])](ranks), genome andThen genomeValues) apply population
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case (o1, o2) =>
          def gv1 = o1.map(math.clamp(_))
          def gv2 = o2.map(math.clamp(_))
          Vector(buildGenome(gv1), buildGenome(gv2))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
    } yield sizedOffspringGenomes
  }

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(lambda: Int, operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperators[M, I](
        tournament[M, I, (Lazy[Int], Lazy[Double])](ranks),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration
      ) apply population
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

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(mu: Int) = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, fitness)
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply cloneRemoved
      elite = keepHighestRanked(cloneRemoved, ranks, mu)
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def step[M[_]: cats.Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] =
    deterministicStep(breeding, expression, elitism)

}
