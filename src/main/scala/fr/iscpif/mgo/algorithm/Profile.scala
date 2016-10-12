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
import fr.iscpif.mgo
import fr.iscpif.mgo._
import ranking._
import niche._
import contexts._
import elitism._
import breeding._
import GenomeVectorDouble._

import scala.util.Random
import scalaz._
import Scalaz._

object profile extends niche.Imports {

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness) }

  def genomeProfile(x: Int, nX: Int): Niche[Individual, Int] =
    genomeProfile[Individual]((Individual.genome composeLens vectorValues).get _, x, nX)

  @Lenses case class Genome(values: Array[Double], operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, fitness: Double, age: Long)

  def vectorValues = Genome.values composeLens arrayToVectorLens

  def buildGenome(values: Vector[Double], operator: Maybe[Int]) = Genome(values.toArray, operator)
  def buildIndividual(g: Genome, fitness: Double) = Individual(g, fitness, 0)

  def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](buildGenome)(lambda, genomeSize)

  def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
    profileOperations.breeding[EvolutionState[Unit, ?], Individual, Genome](
      Individual.fitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda, niche, operatorExploration)

  def expression(fitness: Vector[Double] => Double): Expression[Genome, Individual] =
    profileOperations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism(niche: Niche[Individual, Int]): Elitism[EvolutionState[Unit, ?], Individual] =
    profileOperations.elitism[EvolutionState[Unit, ?], Individual](
      Individual.fitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(niche)

  object Profile {
    implicit def isAlgorithm = new Algorithm[Profile, EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {
      def initialState(t: Profile, rng: Random) = EvolutionData[Unit](random = rng, s = ())

      def initialPopulation(t: Profile): EvolutionState[Unit, Vector[Individual]] =
        deterministicInitialPopulation[EvolutionState[Unit, ?], Genome, Individual](
          profile.initialGenomes(t.lambda, t.genomeSize),
          profile.expression(t.fitness))

      def step(t: Profile): Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
        profileOperations.step[EvolutionState[Unit, ?], Individual, Genome](
          profile.breeding(t.lambda, t.niche, t.operatorExploration),
          profile.expression(t.fitness),
          profile.elitism(t.niche))

      def run[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (EvolutionData[Unit], A) = mgo.unwrap(x, s)
    }
  }

  case class Profile(lambda: Int, fitness: Vector[Double] => Double, niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double = 0.1)

  case class OpenMOLE(niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double)

  object OpenMOLE {

    implicit def integration = new openmole.Integration[OpenMOLE, Vector[Double], Double] with openmole.Profile[OpenMOLE] {
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
        def phenotype(individual: I): Double = Individual.fitness.get(individual)
        def buildIndividual(genome: G, phenotype: Double) = Individual(genome, phenotype, 0)
        def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
        def initialGenomes(n: Int): EvolutionState[Unit, Vector[G]] = fr.iscpif.mgo.algorithm.profile.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], I, G] = fr.iscpif.mgo.algorithm.profile.breeding(n, om.niche, om.operatorExploration)
        def elitism: Elitism[EvolutionState[Unit, ?], I] = fr.iscpif.mgo.algorithm.profile.elitism(om.niche)
        def migrateToIsland(population: Vector[I]) = population
        def migrateFromIsland(population: Vector[I]) = population
      }

      def unwrap[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (S, A) = mgo.unwrap(x, s)

      def profile(om: OpenMOLE)(population: Vector[I]) = population
    }
  }

}

object profileOperations {

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    fitness: I => Double,
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double): Breeding[M, I, G] =
    for {
      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
      gs <- tournament(
        profileRanking[M, I](niche, fitness),
        lambda + 1,
        rounds = size => math.round(math.log10(size).toInt)
      ) andThen
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
    build: (G, Double) => I)(fitness: Vector[Double] => Double): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)))

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    fitness: I => Double,
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(niche: Niche[I, Int]): Elitism[M, I] =
    applyCloneStrategy(values, keepYoungest[M, I](age.get)) andThen
      filterNaN(fitness) andThen
      keepNiches(
        niche = niche,
        objective = minimiseO[M, I, Double](fitness, 1)
      ) andThen incrementGeneration(age)

  def step[M[_]: Monad: RandomGen: Generational, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = deterministicStep(breeding, expression, elitism)
}
