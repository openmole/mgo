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

import scala.language.higherKinds

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._

import scala.math._
import scala.util.Random

import scalaz._
import Scalaz._

object NSGA2 {

  // Les fonctions breeding et elitism définies dans les objets respectifs aux algos doivent être indépendantes des
  // types pour pouvoir être réutilisées ensuite dans d'autres algos. L'algorithme pur (ici NSGA2) est réellement spécifié
  // dans la fonction algorithm tout en bas.

  def initialGenomes[M[_], G](cons: (Vector[Double], Maybe[Int], Long) => G)(mu: Int, genomeSize: Int)(
    implicit MM: Monad[M], MR: RandomGen[M]): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      gs = values.map { (vs: Vector[Double]) => cons(vs, Maybe.empty, 0) }
    } yield gs

  def breeding[M[_], I, G](
    iFitness: Lens[I, Vector[Double]],
    iGenome: Lens[I, G],
    gValues: Lens[G, Vector[Double]],
    gOperator: Lens[G, Maybe[Int]],
    gCons: (Vector[Double], Maybe[Int], Long) => G)(
      lambda: Int,
      operatorExploration: Double)(
        implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Breeding[M, I, G] = {
    for {
      // Select lambda parents with minimum pareto rank and maximum crowding diversity
      parents <- tournament[M, I, (Lazy[Int], Lazy[Double])](
        paretoRankingMinAndCrowdingDiversity[M, I] { iFitness.get },
        // We need to always draw a even number of parents, otherwise the last parent will be paired with itself,
        // resulting in no crossover (problem if lambda = 1).
        if (lambda % 2 == 0) lambda else lambda + 1)
      // Compute the proportion of each operator in the population
      opstats <- Kleisli.kleisli[M, Vector[I], Map[Int, Double]] {
        is: Vector[I] => is.map { (iGenome >=> gOperator).get }.collect { case Maybe.Just(op) => op }.groupBy(identity).mapValues(_.length.toDouble / parents.size).point[M]
      }
      // Get the genome values
      parentgenomes <- thenK(mapPureB[M, I, Vector[Double]] { (iGenome >=> gValues).get })(parents)
      // Pair parents together
      couples <- thenK(pairConsecutive[M, Vector[Double]])(parentgenomes)
      // Apply a crossover+mutation operator to each couple. The operator is selected with a probability equal to its proportion in the population.
      // There is a chance equal to operatorExploration to select an operator at random uniformly instead.
      pairedOffspringsAndOps <- thenK(
        mapB[M, (Vector[Double], Vector[Double]), (((Vector[Double], Vector[Double]), Int), Int)](
          dynamicOperators.selectOperator[M](opstats, operatorExploration).run))(couples)
      // Flatten the resulting offsprings and assign their respective operator to each
      offspringsAndOps <- thenK(flatMapPureB[M, (((Vector[Double], Vector[Double]), Int), Int), (Vector[Double], Int)] {
        case (((g1, g2), op), _) => Vector((g1, op), (g2, op))
      })(pairedOffspringsAndOps)
      // Since we drew a even number of parents, we got an even number of offsprings. If lambda is odd, delete one
      // offspring at random.
      offspringsAndOpsLambdaAdjusted <- thenK(Breeding.apply[M, (Vector[Double], Int), (Vector[Double], Int)] { gs: Vector[(Vector[Double], Int)] =>
        if (lambda % 2 == 0) gs.point[M]
        else
          for {
            rg <- MR.get
          } yield {
            val selected = rg.nextInt(gs.size)
            gs.take(selected) ++ gs.drop(selected + 1)
          }
      })(offspringsAndOps)
      // Clamp genome values between 0 and 1
      clamped <- thenK(mapPureB[M, (Vector[Double], Int), (Vector[Double], Int)] {
        Lens.firstLens[Vector[Double], Int] =>= { _ map { x: Double => max(0.0, min(1.0, x)) } }
      })(offspringsAndOpsLambdaAdjusted)
      // Add the current generation number to new offsprings
      offspringsOpsGens <- thenK(mapB[M, (Vector[Double], Int), (Vector[Double], Int, Long)] {
        case (g, op) => MG.getGeneration.>>=[(Vector[Double], Int, Long)] { gen: Long => (g, op, gen).point[M] }
      })(clamped)
      // Construct the final G type
      gs <- thenK(mapPureB[M, (Vector[Double], Int, Long), G] { case (g, op, gen) => gCons(g, Maybe.just(op), gen) })(offspringsOpsGens)
    } yield gs
  }

  def expression[G, I](
    gValues: Lens[G, Vector[Double]],
    iCons: (G, Vector[Double]) => I)(
      fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues.get(g)))

  def elitism[M[_], I](
    iFitness: Lens[I, Vector[Double]],
    iGenomeValues: Lens[I, Vector[Double]],
    iGeneration: Lens[I, Long])(
      mu: Int)(
        implicit MM: Monad[M], MR: RandomGen[M]): Objective[M, I] =
    for {
      // Declone
      decloned <- applyCloneStrategy[M, I, Vector[Double]](iGenomeValues.get, keepYoungest[M, I] { iGeneration })
      // Filter out NaNs
      noNaNs <- thenK(flatMapPureB[M, I, I] { i: I => if (iGenomeValues.get(i).exists { (_: Double).isNaN }) Vector.empty else Vector(i) })(decloned)
      // Keep the individuals with lowest fitness (pareto) and highest crowding diversity
      is <- thenK(keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](
        paretoRankingMinAndCrowdingDiversity[M, I] { iFitness.get }, mu))(noNaNs)
    } yield is

  def step[M[_], I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Objective[M, I])(
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

    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], generation: Long)
    case class Individual(genome: Genome, fitness: Vector[Double])

    val iFitness: Lens[Individual, Vector[Double]] = Lens.lensu(
      set = (i, v) => i.copy(fitness = v),
      get = _.fitness
    )
    val iGenome: Lens[Individual, Genome] = Lens.lensu(
      set = (i, g) => i.copy(genome = g),
      get = _.genome
    )
    val gValues: Lens[Genome, Vector[Double]] = Lens.lensu(
      set = (g, v) => g.copy(values = v),
      get = _.values
    )
    val gOperator: Lens[Genome, Maybe[Int]] = Lens.lensu(
      set = (g, o) => g.copy(operator = o),
      get = _.operator
    )
    val gGeneration: Lens[Genome, Long] = Lens.lensu(
      set = (g, e) => g.copy(generation = e),
      get = _.generation
    )
    val iGenomeValues: Lens[Individual, Vector[Double]] = iGenome >=> gValues
    val iGeneration: Lens[Individual, Long] = iGenome >=> gGeneration

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
      NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l, Genome](Genome)(mu, genomeSize)
    def breeding(lambda: Int, operatorExploration: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] =
      NSGA2.breeding[EvolutionStateMonad[Unit]#l, Individual, Genome](
        iFitness, iGenome, gValues, gOperator, Genome
      )(lambda, operatorExploration)
    def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
      NSGA2.expression[Genome, Individual](gValues, Individual)(fitness)
    def elitism(mu: Int): Objective[EvolutionStateMonad[Unit]#l, Individual] =
      NSGA2.elitism[EvolutionStateMonad[Unit]#l, Individual](iFitness, iGenomeValues, iGeneration)(mu)

    def step(
      mu: Int,
      lambda: Int,
      fitness: Expression[Vector[Double], Vector[Double]],
      operatorExploration: Double): Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
      NSGA2.step[EvolutionStateMonad[Unit]#l, Individual, Genome](
        breeding(lambda, operatorExploration),
        expression(fitness),
        elitism(mu)
      )

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(mu: Int, lambda: Int, fitness: Vector[Double] => Vector[Double], genomeSize: Int, operatorExploration: Double) =
      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = NSGA2.Algorithm.initialGenomes(mu, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = NSGA2.Algorithm.breeding(lambda, operatorExploration)
        def expression: Expression[Genome, Individual] = NSGA2.Algorithm.expression(fitness)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NSGA2.Algorithm.elitism(mu)

        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] = NSGA2.Algorithm.step(mu, lambda, fitness, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NSGA2.Algorithm.unwrap(x)

      }

    def algoOpenMOLE(mu: Int, genomeSize: Int, operatorExploration: Double) =
      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Genome, EvolutionData[Unit]] {

        val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
          set = (e, r) => e.copy(random = r),
          get = _.random
        )

        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Genome]] = NSGA2.Algorithm.initialGenomes(n, genomeSize)
        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = NSGA2.Algorithm.breeding(n, operatorExploration)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NSGA2.Algorithm.elitism(mu)

        def initForIsland(i: Individual): Individual = i

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NSGA2.Algorithm.unwrap(x)

      }

  }
}
