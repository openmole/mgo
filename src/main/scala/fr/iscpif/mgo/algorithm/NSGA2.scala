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
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.Contexts._

import scalaz._
import Scalaz._
import GenomeVectorDouble._

object NSGA2 {

  // Les fonctions breeding et elitism définies dans les objets respectifs aux algos doivent être indépendantes des
  // types pour pouvoir être réutilisées ensuite dans d'autres algos. L'algorithme pur (ici NSGA2) est réellement spécifié
  // dans la fonction algorithm tout en bas.

  def initialGenomes[M[_], G](cons: (Vector[Double], Maybe[Int]) => G)(mu: Int, genomeSize: Int)(
    implicit MM: Monad[M], MR: RandomGen[M]): M[Vector[G]] =
    for {
      values <- randomGenomes[M](mu, genomeSize)
      gs = values.map { (vs: Vector[Double]) => cons(vs, Maybe.empty) }
    } yield gs

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
    build: (G, Vector[Double], Long) => I)(fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)), 0)

  def elitism[M[_]: Monad: RandomGen, I](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    generation: monocle.Lens[I, Long])(mu: Int): Elitism[M, I] =
    applyCloneStrategy(values, keepYoungest[M, I](generation.get)) andThen
      filterNaN(values) andThen
      keepHighestRankedO(paretoRankingMinAndCrowdingDiversity[M, I](fitness), mu) andThen
      incrementGeneration(generation)

  def step[M[_]: Monad: RandomGen, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I])(implicit MG: Generational[M]): Kleisli[M, Vector[I], Vector[I]] =
    stepEA[M, I, G](
      { (_: Vector[I]) => MG.incrementGeneration },
      breeding,
      expression,
      elitism,
      muPlusLambda[I])

  /** The default NSGA2 algorithm */
  object Algorithm {

    import fr.iscpif.mgo.Contexts.default._

    @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
    @Lenses case class Individual(genome: Genome, fitness: Vector[Double], generation: Long)

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
      NSGA2.initialGenomes[EvolutionState[Unit, ?], Genome](Genome.apply)(mu, genomeSize)

    def breeding(lambda: Int, operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
      NSGA2.breeding[EvolutionState[Unit, ?], Individual, Genome](
        Individual.fitness.get, Individual.genome.get, Genome.values.get, Genome.operator.get, Genome.apply
      )(lambda, operatorExploration)

    def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
      NSGA2.expression[Genome, Individual](Genome.values.get, Individual.apply)(fitness)

    def elitism(mu: Int): Elitism[EvolutionState[Unit, ?], Individual] =
      NSGA2.elitism[EvolutionState[Unit, ?], Individual](Individual.fitness.get, (Individual.genome composeLens Genome.values).get, Individual.generation)(mu)

    def step(
      mu: Int,
      lambda: Int,
      fitness: Expression[Vector[Double], Vector[Double]],
      operatorExploration: Double): Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
      NSGA2.step[EvolutionState[Unit, ?], Individual, Genome](
        breeding(lambda, operatorExploration),
        expression(fitness),
        elitism(mu)
      )

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(mu: Int, lambda: Int, fitness: Vector[Double] => Vector[Double], genomeSize: Int, operatorExploration: Double) =
      new Algorithm[EvolutionState[Unit, ?], Individual, Genome, (EvolutionData[Unit], ?)] {

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = NSGA2.Algorithm.initialGenomes(mu, genomeSize)
        def breeding: Breeding[EvolutionState[Unit, ?], Individual, Genome] = NSGA2.Algorithm.breeding(lambda, operatorExploration)
        def expression: Expression[Genome, Individual] = NSGA2.Algorithm.expression(fitness)
        def elitism: Elitism[EvolutionState[Unit, ?], Individual] = NSGA2.Algorithm.elitism(mu)

        def step: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] = NSGA2.Algorithm.step(mu, lambda, fitness, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NSGA2.Algorithm.unwrap(x)
      }

    def algoOpenMOLE(mu: Int, genomeSize: Int, operatorExploration: Double) =
      new AlgorithmOpenMOLE[EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {

        def cRandom = GenLens[EvolutionData[Unit]](_.random)

        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Genome]] = NSGA2.Algorithm.initialGenomes(n, genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], Individual, Genome] = NSGA2.Algorithm.breeding(n, operatorExploration)
        def elitism: Elitism[EvolutionState[Unit, ?], Individual] = NSGA2.Algorithm.elitism(mu)

        def initForIsland(i: Individual): Individual = i

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NSGA2.Algorithm.unwrap(x)
      }

  }
}
