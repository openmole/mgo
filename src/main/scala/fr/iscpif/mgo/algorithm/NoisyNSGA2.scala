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

import scala.util.Random

import scalaz._
import Scalaz._

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._

import scala.math._

import scala.language.higherKinds

object NoisyNSGA2 {

  def fitnessWithReplications[I](
    iHistory: Lens[I, Vector[Vector[Double]]])(i: I): Vector[Double] = iHistory.get(i).transpose.map { vs => vs.sum / vs.size } ++ Vector(1.0 / iHistory.get(i).size.toDouble)

  def initialGenomes[M[_], I](
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Vector[Double]]) => I)(mu: Int, genomeSize: Int)(
      implicit MM: Monad[M], MR: RandomGen[M], IH: History[Vector[Double], I]): M[Vector[I]] =
    for {
      rgs <- MR.split.replicateM(mu)
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = values.map { vs: Vector[Double] => iCons(vs, Maybe.empty, 1, Vector.empty) }
    } yield indivs

  def breeding[M[_], I](
    iHistory: Lens[I, Vector[Vector[Double]]],
    iValues: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long],
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Vector[Double]]) => I)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double)(
        implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Breeding[M, I, I] =
    for {
      // Select parents with the double objective of minimising fitness and maximising history size with a pareto ranking, and then maximising diversity
      parents <- tournament[M, I, (Lazy[Int], Lazy[Double])](
        paretoRankingMinAndCrowdingDiversity[M, I] { fitnessWithReplications(iHistory) },
        if (lambda % 2 == 0) lambda else lambda + 1)
      // Compute the proportion of each operator in the population
      opstats <- Kleisli.kleisli[M, Vector[I], Map[Int, Double]] {
        is: Vector[I] => is.map { iOperator.get }.collect { case Maybe.Just(op) => op }.groupBy(identity).mapValues(_.length.toDouble / parents.size).point[M]
      }
      // Get the genome values
      parentgenomes <- thenK(mapPureB[M, I, Vector[Double]] { iValues.get })(parents)
      // Pair parent genomes together
      couples <- thenK(pairConsecutive[M, Vector[Double]])(parentgenomes)
      // Apply a crossover+mutation operator to each couple. The operator is selected with a probability equal to its proportion in the population.
      // There is a chance equal to operatorExploration to select an operator at random uniformly instead.
      pairedOffspringsAndOps <- thenK(
        mapB[M, (Vector[Double], Vector[Double]), (((Vector[Double], Vector[Double]), Int), Int)](
          probabilisticOperatorB[M, (Vector[Double], Vector[Double]), ((Vector[Double], Vector[Double]), Int)](
            Vector(
              // This is the operator with probability distribution equal to the proportion in the population
              (probabilisticOperatorB[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])](
                dynamicOperators.crossoversAndMutations[M].zipWithIndex.map {
                  case (op, index) => (op, opstats.getOrElse(index, 0.0))
                }),
                1 - operatorExploration),
              // This is the operator drawn with a uniform probability distribution.
              (probabilisticOperatorB[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])](
                dynamicOperators.crossoversAndMutations[M].zipWithIndex.map {
                  case (op, index) => (op, 1.0 / opstats.size.toDouble)
                }),
                operatorExploration))).run))(couples)
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
      // Construct the final I type
      is <- thenK(mapPureB[M, (Vector[Double], Int), I] { case (g, op) => iCons(g, Maybe.just(op), 0.toLong, Vector.empty) })(clamped)
      // Replace some offsprings by clones from the original population.
      // TODO: les clones sont tirés aléatoirement de la population initiale, pas de tirage par tournoi. Est-ce qu'il
      // faut biaiser le choix des clones par meilleure fitness et historique plus court?
      result <- clonesReplace[M, I, I](Kleisli.kleisli[M, I, I] { (i: I) => iAge.mod({ _ + 1 }, i).point[M] }, cloneProbability)(is)
    } yield result

  def expression[I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]])(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, I), I] =
    { case (rg, i) => iHistory.mod(_ :+ fitness(rg, iValues.get(i)), i) }

  def elitism[M[_], I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long])(mu: Int, historySize: Int)(
      implicit MM: Monad[M], MR: RandomGen[M]): Objective[M, I] =
    for {
      // Declone
      decloned <- applyCloneStrategy[M, I, Vector[Double]](
        iValues.get,
        mergeHistories[M, I, Vector[Double]](iAge, iHistory)(historySize))
      // Filter out NaNs
      noNaN = (decloned: Vector[I]).filterNot { iValues.get(_).exists { (_: Double).isNaN } }
      // Keep the individuals with lowest fitness (pareto) and highest crowding diversity
      kept <- thenK(keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](
        paretoRankingMinAndCrowdingDiversity[M, I] { fitnessWithReplications(iHistory) },
        mu))(noNaN)
    } yield kept

  def step[M[_], I, G](
    breeding: Breeding[M, I, (Random, G)],
    expression: Expression[(Random, G), I],
    elitism: Objective[M, I])(
      implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Kleisli[M, Vector[I], Vector[I]] =
    stepEA[M, I, (Random, G)](
      { (_: Vector[I]) => MG.incrementGeneration },
      breeding,
      expression,
      elitism,
      muPlusLambda[I])

  object Algorithm {
    case class Individual(genome: Vector[Double], operator: Maybe[Int], age: Long, fitnessHistory: Vector[Vector[Double]])

    val iValues: Lens[Individual, Vector[Double]] = Lens.lensu(
      set = (i, v) => i.copy(genome = v),
      get = _.genome
    )
    val iOperator: Lens[Individual, Maybe[Int]] = Lens.lensu(
      set = (i, o) => i.copy(operator = o),
      get = _.operator
    )
    val iAge: Lens[Individual, Long] = Lens.lensu(
      set = (i, a) => i.copy(age = a),
      get = _.age
    )
    val iHistory: Lens[Individual, Vector[Vector[Double]]] = Lens.lensu(
      set = (i, h) => i.copy(fitnessHistory = h),
      get = _.fitnessHistory
    )

    implicit val individualHistory = new History[Vector[Double], Individual] {
      val lens = iHistory
    }

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Individual]] =
      NoisyNSGA2.initialGenomes[EvolutionStateMonad[Unit]#l, Individual](Individual)(mu, genomeSize)
    def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] =
      NoisyNSGA2.breeding[EvolutionStateMonad[Unit]#l, Individual](
        iHistory, iValues, iOperator, iAge, Individual
      )(lambda, operatorExploration, cloneProbability)
    def expression(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, Individual), Individual] =
      NoisyNSGA2.expression[Individual](iValues, iHistory)(fitness)
    def elitism(mu: Int, historySize: Int): Objective[EvolutionStateMonad[Unit]#l, Individual] =
      NoisyNSGA2.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iHistory, iOperator, iAge)(mu, historySize)

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(
      mu: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Vector[Double],
      operatorExploration: Double,
      genomeSize: Int,
      historySize: Int,
      cloneProbability: Double) =
      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] =
          for {
            ig <- NoisyNSGA2.Algorithm.initialGenomes(mu, genomeSize)
            //Add an independant random number generator to each individual
            result <- withRandomGenB[EvolutionStateMonad[Unit]#l, Individual].run(ig)
          } yield result

        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] =
          for {
            bred <- NoisyNSGA2.Algorithm.breeding(lambda, operatorExploration, cloneProbability)
            //Add an independant random number generator to each individual
            result <- thenK[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual], Vector[(Random, Individual)]](withRandomGenB[EvolutionStateMonad[Unit]#l, Individual])(bred)
          } yield result

        def expression: Expression[(Random, Individual), Individual] = NoisyNSGA2.Algorithm.expression(fitness)

        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.Algorithm.elitism(mu, historySize)

        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
          NoisyNSGA2.step[EvolutionStateMonad[Unit]#l, Individual, Individual](
            breeding,
            expression,
            elitism)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyNSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyNSGA2.Algorithm.unwrap(x)
      }

    def algoOpenMOLE(mu: Int, operatorExploration: Double, genomeSize: Int, historySize: Int, cloneProbability: Double) =
      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Individual, EvolutionData[Unit]] {

        lazy val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
          set = (e, r) => e.copy(random = r),
          get = _.random
        )

        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Individual]] = NoisyNSGA2.Algorithm.initialGenomes(n, genomeSize)
        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] = NoisyNSGA2.Algorithm.breeding(n, operatorExploration, cloneProbability)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.Algorithm.elitism(mu, historySize)

        def initForIsland(i: Individual): Individual = i.copy(age = 0)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyNSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyNSGA2.Algorithm.unwrap(x)

      }
  }
}

