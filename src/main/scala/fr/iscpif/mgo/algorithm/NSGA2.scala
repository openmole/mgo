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

import monocle.macros.{ GenLens, Lenses }

import scala.language.higherKinds

import fr.iscpif.mgo
import fr.iscpif.mgo._

import ranking._
import breeding._
import elitism._
import contexts._

import scala.util.Random
import scalaz._
import Scalaz._
import GenomeVectorDouble._

object nsga2 {

  @Lenses case class Genome(values: Array[Double], operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, fitness: Array[Double], age: Long)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)
  def buildGenome(values: Vector[Double], operator: Maybe[Int]) = Genome(values.toArray, operator)

  def vectorFitness = Individual.fitness composeLens arrayToVectorLens
  def vectorValues = Genome.values composeLens arrayToVectorLens

  def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](buildGenome)(lambda, genomeSize)

  def breeding(lambda: Int, operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
    nsga2Operations.breeding[EvolutionState[Unit, ?], Individual, Genome](
      vectorFitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda, operatorExploration)

  def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    nsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism(mu: Int): Elitism[EvolutionState[Unit, ?], Individual] =
    nsga2Operations.elitism[EvolutionState[Unit, ?], Individual](
      vectorFitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(mu)

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness.toVector) }

  object NSGA2 {

    implicit def isAlgorithm: Algorithm[NSGA2, EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] =
      new Algorithm[NSGA2, EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {
        override def initialState(t: NSGA2, rng: Random): EvolutionData[Unit] = EvolutionData[Unit](random = rng, s = ())
        override def initialPopulation(t: NSGA2): EvolutionState[Unit, Vector[Individual]] =
          deterministicInitialPopulation[EvolutionState[Unit, ?], Genome, Individual](nsga2.initialGenomes(t.lambda, t.genomeSize), expression(t.fitness))
        override def step(t: NSGA2): Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
          nsga2Operations.step(nsga2.breeding(t.lambda, t.operatorExploration), nsga2.expression(t.fitness), nsga2.elitism(t.mu))
        override def run[A](m: EvolutionState[Unit, A], ca: EvolutionData[Unit]): (EvolutionData[Unit], A) = mgo.unwrap(m, ca)
      }

  }

  case class NSGA2(mu: Int, lambda: Int, fitness: Vector[Double] => Vector[Double], genomeSize: Int, operatorExploration: Double = 0.1)

  case class OpenMOLE(mu: Int, genomeSize: Int, operatorExploration: Double)

  object OpenMOLE {

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
        def values(genome: G) = vectorValues.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = vectorFitness.get(individual)
        def buildIndividual(genome: G, phenotype: Vector[Double]) = nsga2.buildIndividual(genome, phenotype)
        def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
        def initialGenomes(n: Int): EvolutionState[Unit, Vector[G]] = nsga2.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], I, G] = nsga2.breeding(n, om.operatorExploration)
        def elitism: Elitism[EvolutionState[Unit, ?], I] = nsga2.elitism(om.mu)
        def migrateToIsland(population: Vector[I]) = population
        def migrateFromIsland(population: Vector[I]) = population
      }

      def unwrap[A](x: EvolutionState[Unit, A], s: S): (S, A) = mgo.unwrap(x, s)
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