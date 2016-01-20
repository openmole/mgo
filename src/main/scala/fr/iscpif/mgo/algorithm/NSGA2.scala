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
package fr.iscpif.mgo.algorithm

import monocle.macros.{ Lenses, GenLens }

import scala.language.higherKinds

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.contexts._

import scala.util.Random
import scalaz._
import Scalaz._
import GenomeVectorDouble._

object nsga2 {

  import fr.iscpif.mgo.contexts.default._

  @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, fitness: Vector[Double], age: Long)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f, 0)

  def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](Genome.apply)(lambda, genomeSize)

  def breeding(lambda: Int, operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
    nsga2Operations.breeding[EvolutionState[Unit, ?], Individual, Genome](
      Individual.fitness.get, Individual.genome.get, Genome.values.get, Genome.operator.get, Genome.apply
    )(lambda, operatorExploration)

  def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    nsga2Operations.expression[Genome, Individual](Genome.values.get, buildIndividual)(fitness)

  def elitism(mu: Int): Elitism[EvolutionState[Unit, ?], Individual] =
    nsga2Operations.elitism[EvolutionState[Unit, ?], Individual](
      Individual.fitness.get,
      (Individual.genome composeLens Genome.values).get,
      Individual.age)(mu)

  case class NSGA2(mu: Int, lambda: Int, fitness: Vector[Double] => Vector[Double], genomeSize: Int, operatorExploration: Double) extends Algorithm[EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {
    def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
    def initialGenomes: EvolutionState[Unit, Vector[Genome]] = nsga2.initialGenomes(lambda, genomeSize)
    def breeding: Breeding[EvolutionState[Unit, ?], Individual, Genome] = nsga2.breeding(lambda, operatorExploration)
    def expression: Expression[Genome, Individual] = nsga2.expression(fitness)
    def elitism: Elitism[EvolutionState[Unit, ?], Individual] = nsga2.elitism(mu)

    def step: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
      nsga2Operations.step[EvolutionState[Unit, ?], Individual, Genome](breeding, expression, elitism)

    def run[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (EvolutionData[Unit], A) = default.unwrap(x, s)
  }

  case class OpenMOLE(mu: Int, genomeSize: Int, operatorExploration: Double)

  object OpenMOLE {
    import fr.iscpif.mgo.contexts.default._

    implicit def integration: openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] {
      type M[A] = EvolutionState[Unit, A]
      type G = Genome
      type I = Individual
      type S = EvolutionData[Unit]

      def iManifest = implicitly
      def gManifest = implicitly
      def sManifest = implicitly
      def mMonad = implicitly
      def mGenerational = implicitly
      def mStartTime = implicitly

      def operations(om: OpenMOLE) = new Ops {
        def randomLens = GenLens[S](_.random)
        def startTimeLens = GenLens[S](_.startTime)
        def generation(s: EvolutionData[Unit]) = s.generation
        def values(genome: G) = Genome.values.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = Individual.fitness.get(individual)
        def buildIndividual(genome: G, phenotype: Vector[Double]) = Individual(genome, phenotype, 0)
        def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
        def initialGenomes(n: Int): EvolutionState[Unit, Vector[G]] = nsga2.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], I, G] = nsga2.breeding(n, om.operatorExploration)
        def elitism: Elitism[EvolutionState[Unit, ?], I] = nsga2.elitism(om.mu)
        def migrateToIsland(i: I): I = i
        def migrateFromIsland(population: Vector[I]) = population
      }

      def unwrap[A](x: EvolutionState[Unit, A], s: S): (S, A) = default.unwrap(x, s)
    }

  }
}

object nsga2Operations {
  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      operatorExploration: Double): Breeding[M, I, G] =
    for {
      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
      gs <- tournament(paretoRankingMinAndCrowdingDiversity[M, I](fitness), lambda + 1) andThen
        pairConsecutive andThen
        mapPureB { case (g1, g2) => ((genome andThen genomeValues)(g1), (genome andThen genomeValues)(g2)) } andThen
        applyDynamicOperator(operatorStatistics, operatorExploration) andThen
        flatMapPureB { case ((g1, g2), op) => Vector((g1, op), (g2, op)) } andThen
        randomTakeLambda(lambda) andThen
        clamp(GenLens[(Vector[Double], Int)](_._1)) andThen
        mapPureB { case (g, op) => buildGenome(g, Maybe.just(op)) }
    } yield gs

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Vector[Double]) => I)(fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)))

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(mu: Int): Elitism[M, I] =
    applyCloneStrategy(values, keepYoungest[M, I](age.get)) andThen
      filterNaN(fitness) andThen
      keepHighestRanked(paretoRankingMinAndCrowdingDiversity[M, I](fitness), mu) andThen
      incrementGeneration(age)

  def step[M[_]: Monad: RandomGen: Generational, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = deterministicStep(breeding, expression, elitism)
}