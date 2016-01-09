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

import fr.iscpif.mgo._
import ranking._
import niche._
import contexts._
import elitism._
import breeding._
import GenomeVectorDouble._

import scala.math._
import scala.util.Random
import scalaz._
import Scalaz._

object profile {

  import fr.iscpif.mgo.contexts.default._

  @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, fitness: Double, age: Long)

  def buildIndividual(g: Genome, fitness: Double) = Individual(g, fitness, 0)

  def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](Genome.apply)(lambda, genomeSize)

  def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
    profileOperations.breeding[EvolutionState[Unit, ?], Individual, Genome](
      Individual.fitness.get, Individual.genome.get, Genome.values.get, Genome.operator.get, Genome.apply
    )(lambda, niche, operatorExploration)

  def expression(fitness: Vector[Double] => Double): Expression[Genome, Individual] =
    profileOperations.expression[Genome, Individual](Genome.values.get, buildIndividual)(fitness)

  def elitism(niche: Niche[Individual, Int]): Elitism[EvolutionState[Unit, ?], Individual] =
    profileOperations.elitism[EvolutionState[Unit, ?], Individual](
      Individual.fitness.get,
      (Individual.genome composeLens Genome.values).get,
      Individual.age)(niche)

  case class Profile(lambda: Int, fitness: Vector[Double] => Double, niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double) extends Algorithm[EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {
    def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
    def initialGenomes: EvolutionState[Unit, Vector[Genome]] = profile.initialGenomes(lambda, genomeSize)
    def breeding: Breeding[EvolutionState[Unit, ?], Individual, Genome] = profile.breeding(lambda, niche, operatorExploration)
    def expression: Expression[Genome, Individual] = profile.expression(fitness)
    def elitism: Elitism[EvolutionState[Unit, ?], Individual] = profile.elitism(niche)

    def step: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
      profileOperations.step[EvolutionState[Unit, ?], Individual, Genome](breeding, expression, elitism)

    def run[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (EvolutionData[Unit], A) = default.unwrap(x, s)
  }

  case class OpenMOLE(niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double)

  object OpenMOLE {

    import fr.iscpif.mgo.contexts.default._

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
        def randomLens = GenLens[EvolutionData[Unit]](_.random)
        def generation(s: EvolutionData[Unit]) = s.generation
        def values(genome: G) = Genome.values.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Double = Individual.fitness.get(individual)
        def buildIndividual(genome: G, phenotype: Double) = Individual(genome, phenotype, 0)
        def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
        def initialGenomes(n: Int): EvolutionState[Unit, Vector[G]] = fr.iscpif.mgo.algorithm.profile.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], I, G] = fr.iscpif.mgo.algorithm.profile.breeding(n, om.niche, om.operatorExploration)
        def elitism: Elitism[EvolutionState[Unit, ?], I] = fr.iscpif.mgo.algorithm.profile.elitism(om.niche)
        def migrateToIsland(i: I): I = i
      }

      def unwrap[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (S, A) = default.unwrap(x, s)

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
