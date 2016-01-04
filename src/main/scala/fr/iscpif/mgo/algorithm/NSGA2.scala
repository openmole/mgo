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

  def breeding[M[_], I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      operatorExploration: Double)(
        implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Breeding[M, I, G] = {
    for {
      // Compute the proportion of each operator in the population
      opstats <- operatorProportions[M, I](genome andThen genomeOperator)
      // Select lambda parents with minimum pareto rank and maximum crowding diversity
      parents <- tournament[M, I, (Lazy[Int], Lazy[Double])](
        paretoRankingMinAndCrowdingDiversity[M, I](fitness),
        // We need to always draw a even number of parents, otherwise the last parent will be paired with itself,
        // resulting in no crossover (problem if lambda = 1).
        if (lambda % 2 == 0) lambda else lambda + 1)
      // Get the genome values
      parentgenomes <- thenK(mapPureB[M, I, Vector[Double]] { genome andThen genomeValues })(parents)
      // Pair parents together
      couples <- thenK(pairConsecutive[M, Vector[Double]])(parentgenomes)
      // Apply a crossover+mutation operator to each couple. The operator is selected with a probability equal to its proportion in the population.
      // There is a chance equal to operatorExploration to select an operator at random uniformly instead.
      pairedOffspringsAndOps <- thenK(
        mapB[M, (Vector[Double], Vector[Double]), ((Vector[Double], Vector[Double]), Int)](
          dynamicOperators.selectOperator[M](opstats, operatorExploration).run))(couples)
      // Flatten the resulting offsprings and assign their respective operator to each
      offspringsAndOps <- thenK(flatMapPureB[M, ((Vector[Double], Vector[Double]), Int), (Vector[Double], Int)] {
        case ((g1, g2), op) => Vector((g1, op), (g2, op))
      })(pairedOffspringsAndOps)
      offspringsAndOpsLambdaAdjusted <- thenK(randomTakeLambda[M, (Vector[Double], Int)](lambda))(offspringsAndOps)
      // Clamp genome values between 0 and 1
      clamped <- thenK(clamp[M, (Vector[Double], Int)](GenLens[(Vector[Double], Int)](_._1)))(offspringsAndOpsLambdaAdjusted)
      // Construct the final G type
      gs <- thenK(mapPureB[M, (Vector[Double], Int), G] { case (g, op) => buildGenome(g, Maybe.just(op)) })(clamped)
    } yield gs
  }

  def expression[G, I](
    gValues: G => Vector[Double],
    iCons: (G, Vector[Double], Long) => I)(fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues(g)), 0)

  def elitism[M[_], I](
    iFitness: I => Vector[Double],
    iGenomeValues: I => Vector[Double],
    iGeneration: monocle.Lens[I, Long])(
      mu: Int)(
        implicit MM: Monad[M], MR: RandomGen[M]): Elitism[M, I] =
    for {
      // Declone
      p1 <- applyCloneStrategy[M, I, Vector[Double]](iGenomeValues, keepYoungest[M, I] { iGeneration.get })
      p2 <- thenK(filterNaN[M, I](iGenomeValues))(p1)
      // Keep the individuals with lowest fitness (pareto) and highest crowding diversity
      p3 <- thenK(keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](paretoRankingMinAndCrowdingDiversity[M, I] { iFitness }, mu))(p2)
      p4 <- thenK(incrementGeneration[M, I](iGeneration))(p3)
    } yield p4

  def step[M[_], I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I])(
      implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Kleisli[M, Vector[I], Vector[I]] =
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
