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

import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.niche._

import scala.math._
import scala.util.Random
import scalaz._
import Scalaz._

object NoisyProfile {

  def fitnessWithReplications[I](
    iFitness: Lens[I, Double],
    iHistory: Lens[I, Vector[Double]])(i: I): Vector[Double] = Vector(iFitness.get(i), 1.0 / iHistory.get(i).size)

  def initialGenomes[M[_]: Monad: RandomGen, I](
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Double]) => I)(mu: Int, genomeSize: Int): M[Vector[(Random, I)]] =
    for {
      rgs <- implicitly[RandomGen[M]].split.replicateM(mu)
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = rgs.toVector zip values.map { (vs: Vector[Double]) => iCons(vs, Maybe.empty, 1, Vector.empty) }
    } yield indivs

  def breeding[M[_]: Monad: RandomGen: Generational, I](
    iFitness: Lens[I, Double],
    iHistory: Lens[I, Vector[Double]],
    iValues: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long],
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Double]) => I)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double,
      cloneProbability: Double): Breeding[I, M, (Random, I)] =
    withRandomGenB[I, M, I](
      (individuals: Vector[I]) => {
        for {
          rg <- implicitly[RandomGen[M]].split
          generation <- implicitly[Generational[M]].getGeneration
          selected <- tournament[I, (Lazy[Int], Lazy[Double]), M](
            ranking = paretoRankingMinAndCrowdingDiversity[I] { fitnessWithReplications(iFitness, iHistory) }(rg),
            size = lambda,
            rounds = size => math.round(math.log10(size).toInt))(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
          bred <- asB[I, (I, Maybe[Int]), M, (I, Maybe[Int]), I](
            { (i: I) => (i, iOperator.get(i)) },
            { case (i, op) => iOperator.set(i, op) },
            dynamicallyOpB[I, M, I, (I, I), (I, I)](
              pairConsecutive[I, M],
              { case (i1, i2) => Vector(i1, i2).point[M] },
              dynamicOperators.crossoversAndMutations[M].map {
                op =>
                  opOrClone[(I, I), M, (I, I)](
                    // cloning copies the whole individual as is and increments its age
                    clone = {
                      case (i1, i2) =>
                        def age(i: I): I = iAge.mod(_ + 1, i)
                        (age(i1), age(i2))
                    },
                    op = {
                      case (i1, i2) =>
                        for {
                          newg1g2 <- op(iValues.get(i1), iValues.get(i2))
                          (newg1, newg2) = newg1g2
                          newi1 = iHistory.set(iValues.set(i1, newg1), Vector.empty)
                          newi2 = iHistory.set(iValues.set(i2, newg2), Vector.empty)
                        } yield (newi1, newi2)
                    },
                    cloneProbability = cloneProbability)
              },
              operatorExploration))(implicitly[Monad[M]])(selected)
          clamped = (bred: Vector[I]).map { iValues =>= { _ map { x: Double => max(0.0, min(1.0, x)) } } }
        } yield clamped
      }
    )

  def expression[I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Double]])(fitness: (Random, Vector[Double]) => Double): Expression[(Random, I), I] =
    { case (rg, i) => iHistory.mod(_ :+ fitness(rg, iValues.get(i)), i) }

  def elitism[M[_]: Monad: RandomGen, I](
    iValues: Lens[I, Vector[Double]],
    iFitness: Lens[I, Double],
    iHistory: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int], historySize: Int): Objective[I, M] =
    byNicheO[I, Int, M](
      niche = niche,
      objective = (individuals: Vector[I]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[I, Vector[Double], M](
            { (i: I) => iValues.get(i) },
            clonesMergeHistories[I, Double, M](iAge, iHistory)(historySize))(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[I]).filterNot { iValues.get(_).exists { (_: Double).isNaN } }
          kept <- keepHighestRankedO[I, (Lazy[Int], Lazy[Double]), M](
            paretoRankingMinAndCrowdingDiversity[I] { fitnessWithReplications(iFitness, iHistory) }(rg),
            muByNiche)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
        } yield kept
    )

  def step[M[_]: Monad: RandomGen: Generational, I](
    breeding: Breeding[I, M, (Random, I)],
    expression: Expression[(Random, I), I],
    elitism: Objective[I, M]): Vector[I] => M[Vector[I]] =
    stepEA[I, M, (Random, I)](
      { (_: Vector[I]) => implicitly[Generational[M]].incrementGeneration },
      breeding,
      expression,
      elitism,
      muPlusLambda[I])

  object Algorithm {

    case class Individual(
      genome: Vector[Double],
      operator: Maybe[Int],
      age: Long,
      fitnessHistory: Vector[Double])

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
    val iHistory: Lens[Individual, Vector[Double]] = Lens.lensu(
      set = (i, h) => i.copy(fitnessHistory = h),
      get = _.fitnessHistory
    )
    val iFitness: Lens[Individual, Double] = Lens.lensu(
      set = (i, f) => i.copy(fitnessHistory = i.fitnessHistory.dropRight(1) :+ f),
      get = _.fitnessHistory.last
    )

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[(Random, Individual)]] = NoisyProfile.initialGenomes[EvolutionStateMonad[Unit]#l, Individual](Individual)(mu, genomeSize)
    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double): Breeding[Individual, EvolutionStateMonad[Unit]#l, (Random, Individual)] =
      NoisyProfile.breeding[EvolutionStateMonad[Unit]#l, Individual](
        iFitness, iHistory, iValues, iOperator, iAge, Individual
      )(lambda, niche, operatorExploration, cloneProbability)
    def expression(fitness: (Random, Vector[Double]) => Double): Expression[(Random, Individual), Individual] =
      NoisyProfile.expression[Individual](iValues, iHistory)(fitness)
    def elitism(muByNiche: Int, niche: Niche[Individual, Int], historySize: Int): Objective[Individual, EvolutionStateMonad[Unit]#l] =
      NoisyProfile.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iFitness, iHistory, iOperator, iAge)(muByNiche, niche, historySize)

    def step(
      muByNiche: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Double,
      niche: Niche[Individual, Int],
      historySize: Int,
      cloneProbability: Double,
      operatorExploration: Double): Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
      NoisyProfile.step[EvolutionStateMonad[Unit]#l, Individual](
        breeding(lambda, niche, operatorExploration, cloneProbability),
        expression(fitness),
        elitism(muByNiche, niche, historySize)
      )

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(
      muByNiche: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Double,
      niche: Niche[Individual, Int],
      genomeSize: Int,
      historySize: Int,
      operatorExploration: Double,
      cloneProbability: Double) =
      new Algorithm[Individual, EvolutionStateMonad[Unit]#l, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] = NoisyProfile.Algorithm.initialGenomes(muByNiche, genomeSize)
        def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, (Random, Individual)] = NoisyProfile.Algorithm.breeding(lambda, niche, operatorExploration, cloneProbability)
        def expression: Expression[(Random, Individual), Individual] = NoisyProfile.Algorithm.expression(fitness)
        def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NoisyProfile.Algorithm.elitism(muByNiche, niche, historySize)

        def step: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] = NoisyProfile.Algorithm.step(muByNiche, lambda, fitness, niche, historySize, cloneProbability, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyProfile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyProfile.Algorithm.unwrap(x)
      }
  }

  /*def algorithm(muByNiche: Int, lambda: Int, genomeSize: Int, niche: Niche[Individual, Int], historySize: Int, operatorExploration: Double, cloneProbability: Double) =
    new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Individual, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
      implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

      def initialGenomes: EvolutionState[Unit, Vector[Individual]] = NoisyProfile.initialPopulation[EvolutionStateMonad[Unit]#l](muByNiche, genomeSize)
      def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Individual] = NoisyProfile.breeding[EvolutionStateMonad[Unit]#l](lambda, niche, operatorExploration, cloneProbability)
      def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NoisyProfile.elitism[EvolutionStateMonad[Unit]#l](muByNiche, niche, historySize)

      def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
      def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    }*/

}
