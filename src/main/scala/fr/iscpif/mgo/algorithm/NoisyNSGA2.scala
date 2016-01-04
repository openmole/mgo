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

import fr.iscpif.mgo.algorithm.GenomeVectorDouble._
import monocle.macros.{ GenLens }

import scalaz._
import Scalaz._

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._

import scala.language.higherKinds

object NoisyNSGA2 {

  def fitnessWithReplications[I](history: I => Vector[Vector[Double]], historyAggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
    historyAggregation(history(i)) ++ Vector(1.0 / history(i).size.toDouble)

  def initialGenomes[M[_]: Monad: RandomGen, G](gCons: (Vector[Double], Maybe[Int]) => G)(mu: Int, genomeSize: Int): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
    } yield values.map { vs: Vector[Double] => gCons(vs, Maybe.empty) }

  def breeding[M[_]: Monad: RandomGen: Generational, G, I](
    fitness: I => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    history: I => Vector[Vector[Double]],
    historyAggregation: Vector[Vector[Double]] => Vector[Double],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      gs <- NSGA2.breeding[M, I, G](
        fitnessWithReplications(history, historyAggregation),
        genome,
        genomeValues,
        genomeOperator,
        buildGenome
      )(lambda, operatorExploration) andThen clonesReplace(cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: Monad: RandomGen, I](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    generation: monocle.Lens[I, Long],
    age: Lens[I, Long],
    history: Lens[I, Vector[Vector[Double]]],
    historyAggregation: Vector[Vector[Double]] => Vector[Double])(mu: Int, historySize: Int): Elitism[M, I] =
    applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](age, history)(historySize)) andThen
      filterNaN(values) andThen
      keepHighestRankedO(paretoRankingMinAndCrowdingDiversity[M, I](fitnessWithReplications(history.get, historyAggregation)), mu) andThen
      incrementGeneration(generation)

  //  def expression[G, I](
  //    gValues: Lens[G, Vector[Double]],
  //    iCons: (G, Vector[Double]) => I,
  //    iHistory: Lens[I, Vector[Vector[Double]]])(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, G), I] =
  //    { case (rg, g) => iCons(g, fitness(rg, gValues.get(g))) }
  //
  //  def elitism[M[_], I](
  //    iValues: Lens[I, Vector[Double]],
  //    iHistory: Lens[I, Vector[Vector[Double]]],
  //    iOperator: Lens[I, Maybe[Int]],
  //    iAge: Lens[I, Long])(mu: Int, historySize: Int)(
  //      implicit MM: Monad[M], MR: RandomGen[M]): Elitism[M, I] =
  //    for {
  //      // Declone
  //      decloned <- applyCloneStrategy[M, I, Vector[Double]](
  //        iValues.get,
  //        mergeHistories[M, I, Vector[Double]](iAge, iHistory)(historySize))
  //      // Filter out NaNs
  //      noNaN = (decloned: Vector[I]).filterNot { iValues.get(_).exists { (_: Double).isNaN } }
  //      // Keep the individuals with lowest fitness (pareto) and highest crowding diversity
  //      kept <- thenK(keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](
  //        paretoRankingMinAndCrowdingDiversity[M, I] { fitnessWithReplications(iHistory) },
  //        mu))(noNaN)
  //    } yield kept
  //
  //  def step[M[_], I, G](
  //    breeding: Breeding[M, I, (Random, G)],
  //    expression: Expression[(Random, G), I],
  //    elitism: Elitism[M, I])(
  //      implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Kleisli[M, Vector[I], Vector[I]] =
  //    stepEA[M, I, (Random, G)](
  //      { (_: Vector[I]) => MG.incrementGeneration },
  //      breeding,
  //      expression,
  //      elitism,
  //      muPlusLambda[I])

  //  object Algorithm {
  //
  //    @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
  //    @Lenses case class Individual(genome: Genome, age: Long, fitnessHistory: Vector[Vector[Double]], generation: Long)
  //
  //    implicit val individualHistory = new History[Vector[Double], Individual] {
  //      val lens = Individual.fitnessHistory
  //    }
  //
  //    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Individual]] =
  //      NoisyNSGA2.initialGenomes[EvolutionStateMonad[Unit]#l, Genome](Genome)(mu, genomeSize)
  //
  //    def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] =
  //      NoisyNSGA2.breeding[EvolutionStateMonad[Unit]#l, Individual](
  //        iHistory, iValues, iOperator, iAge, Individual
  //      )(lambda, operatorExploration, cloneProbability)
  //    def expression(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, Individual), Individual] =
  //      NoisyNSGA2.expression[Individual](iValues, iHistory)(fitness)
  //    def elitism(mu: Int, historySize: Int): Elitism[EvolutionStateMonad[Unit]#l, Individual] =
  //      NoisyNSGA2.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iHistory, iOperator, iAge)(mu, historySize)
  //
  //    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
  //    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)
  //
  //    def apply(
  //      mu: Int,
  //      lambda: Int,
  //      fitness: (Random, Vector[Double]) => Vector[Double],
  //      operatorExploration: Double,
  //      genomeSize: Int,
  //      historySize: Int,
  //      cloneProbability: Double) =
  //      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {
  //
  //        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] =
  //          for {
  //            ig <- NoisyNSGA2.Algorithm.initialGenomes(mu, genomeSize)
  //            //Add an independant random number generator to each individual
  //            result <- withRandomGenB[EvolutionStateMonad[Unit]#l, Individual].run(ig)
  //          } yield result
  //
  //        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] =
  //          for {
  //            bred <- NoisyNSGA2.Algorithm.breeding(lambda, operatorExploration, cloneProbability)
  //            //Add an independant random number generator to each individual
  //            result <- thenK[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual], Vector[(Random, Individual)]](withRandomGenB[EvolutionStateMonad[Unit]#l, Individual])(bred)
  //          } yield result
  //
  //        def expression: Expression[(Random, Individual), Individual] = NoisyNSGA2.Algorithm.expression(fitness)
  //
  //        def elitism: Elitism[EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.Algorithm.elitism(mu, historySize)
  //
  //        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
  //          NoisyNSGA2.step[EvolutionStateMonad[Unit]#l, Individual, Individual](
  //            breeding,
  //            expression,
  //            elitism)
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyNSGA2.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyNSGA2.Algorithm.unwrap(x)
  //      }
  //
  //    def algoOpenMOLE(mu: Int, operatorExploration: Double, genomeSize: Int, historySize: Int, cloneProbability: Double) =
  //      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Individual, EvolutionData[Unit]] {
  //
  //        lazy val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
  //          set = (e, r) => e.copy(random = r),
  //          get = _.random
  //        )
  //
  //        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Individual]] = NoisyNSGA2.Algorithm.initialGenomes(n, genomeSize)
  //        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] = NoisyNSGA2.Algorithm.breeding(n, operatorExploration, cloneProbability)
  //        def elitism: Elitism[EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.Algorithm.elitism(mu, historySize)
  //
  //        def initForIsland(i: Individual): Individual = i.copy(age = 0)
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyNSGA2.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyNSGA2.Algorithm.unwrap(x)
  //
  //      }
  //  }
}

