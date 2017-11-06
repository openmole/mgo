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
import mgo.tools.math.clamp
import GenomeVectorDouble._
import cats.data._
import cats.implicits._
import mgo.tools.{ Lazy, math }
import freedsl.tool._
import mgo.niche

object profile extends niche.Imports {

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness) }

  def genomeProfile(x: Int, nX: Int): Niche[Individual, Int] =
    genomeProfile[Individual]((Individual.genome composeLens vectorValues).get _, x, nX)

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(genome: Genome, fitness: Double, age: Long)

  def vectorValues = Genome.values composeLens arrayToVectorLens

  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)
  def buildIndividual(g: Genome, fitness: Double) = Individual(g, fitness, 0)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, genomeSize: Int) =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(lambda, genomeSize)

  def breeding[M[_]: cats.Monad: Random: Generation](lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[M, Individual, Genome] =
    profileOperations.breeding[M, Individual, Genome](
      Individual.fitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda, niche, operatorExploration)

  def expression(fitness: Vector[Double] => Double): Expression[Genome, Individual] =
    profileOperations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism[M[_]: cats.Monad: Random: Generation](niche: Niche[Individual, Int]): Elitism[M, Individual] =
    profileOperations.elitism[M, Individual](
      Individual.fitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(niche)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object Profile {

    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

    implicit def isAlgorithm[M[_]: cats.Monad: Generation: Random: StartTime]: Algorithm[Profile, M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[Profile, M, Individual, Genome, EvolutionState[Unit]] {
      def initialPopulation(t: Profile) =
        deterministicInitialPopulation[M, Genome, Individual](
          profile.initialGenomes[M](t.lambda, t.genomeSize),
          profile.expression(t.fitness))

      def step(t: Profile) =
        profileOperations.step[M, Individual, Genome](
          profile.breeding(t.lambda, t.niche, t.operatorExploration),
          profile.expression(t.fitness),
          profile.elitism(t.niche))

      def state = profile.state[M]
    }
  }

  case class Profile(lambda: Int, fitness: Vector[Double] => Double, niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double = 0.1)

}

object profileOperations {

  import scala.math._

  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
    fitness: I => Double,
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- profileRanking[M, I](niche, fitness) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperators[M, I](
        tournament[M, I, Lazy[Int]](ranks, rounds = size => round(log10(size).toInt)),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration
      ) apply population
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case ((o1, o2), op) =>
          def gv1 = o1.map(clamp(_))
          def gv2 = o2.map(clamp(_))
          Vector(buildGenome(gv1, Some(op)), buildGenome(gv2, Some(op)))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
    } yield sizedOffspringGenomes
  }

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Double) => I)(fitness: Vector[Double] => Double): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)))

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    fitness: I => Double,
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(niche: Niche[I, Int]): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, fitness)
      elite <- keepNiches(niche, minimiseO[M, I, Double](fitness, 1)) apply cloneRemoved
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def step[M[_]: cats.Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = deterministicStep(breeding, expression, elitism)
}
