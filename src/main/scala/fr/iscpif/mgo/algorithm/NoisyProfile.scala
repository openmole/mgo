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

  type V = Vector[Double]
  case class Individual(
    genome: V,
    operator: Maybe[Int],
    age: Long,
    fitnessHistory: Vector[Double])

  implicit val individualAge: Age[Individual] = new Age[Individual] {
    def getAge(i: Individual): Long = i.age
    def setAge(i: Individual, a: Long): Individual = i.copy(age = a)
  }

  implicit val individualHistory: PhenotypeHistory[Individual, Double] = new PhenotypeHistory[Individual, Double] {
    def getPhenotype(i: Individual): Double = i.fitnessHistory.last
    def append(i: Individual, p: Double): Individual = i.copy(fitnessHistory = i.fitnessHistory :+ p)
    def getHistory(i: Individual): Vector[Double] = i.fitnessHistory
    def setHistory(i: Individual, h: Vector[Double]): Individual = i.copy(fitnessHistory = h)
  }

  def fitnessWithReplications(i: Individual): Vector[Double] = Vector(individualHistory.getPhenotype(i), 1.0 / individualHistory.getHistory(i).size)

  def initialPopulation[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Individual]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = values.map { (vs: Vector[Double]) => Individual(genome = vs, operator = Maybe.empty, age = 1, fitnessHistory = Vector.empty) }
    } yield indivs

  def breeding[M[_]: Monad: RandomGen: Generational](
    lambda: Int,
    niche: Niche[Individual, Int],
    operationExploration: Double,
    cloneProbability: Double): Breeding[Individual, M, Individual] =
    (individuals: Vector[Individual]) => {
      for {
        rg <- implicitly[RandomGen[M]].split
        generation <- implicitly[Generational[M]].getGeneration
        selected <- tournament[Individual, (Lazy[Int], Lazy[Double]), M](
          ranking = paretoRankingMinAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
          size = lambda,
          rounds = size => math.round(math.log10(size).toInt))(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
        bred <- asB[Individual, (Individual, Maybe[Int]), M, (Individual, Maybe[Int]), Individual](
          { (i: Individual) => (i, i.operator) },
          { case (i: Individual, op: Maybe[Int]) => i.copy(operator = op) },
          dynamicallyOpB[Individual, M, Individual, (Individual, Individual), (Individual, Individual)](
            pairConsecutive[Individual, M],
            { case (i1, i2) => Vector(i1, i2).point[M] },
            dynamicOperators.crossoversAndMutations[M].map {
              op =>
                opOrClone[(Individual, Individual), M, (Individual, Individual)](
                  // cloning copies the whole individual as is and increments its age
                  clone = {
                    case (i1: Individual, i2: Individual) =>
                      def age(i: Individual): Individual = i.copy(age = i.age + 1)
                      (age(i1), age(i2))
                  },
                  op = {
                    case (i1: Individual, i2: Individual) =>
                      for {
                        newg1g2 <- op(i1.genome, i2.genome)
                        (newg1, newg2) = newg1g2
                        newi1 = i1.copy(genome = newg1, fitnessHistory = Vector.empty)
                        newi2 = i2.copy(genome = newg2, fitnessHistory = Vector.empty)
                      } yield (newi1, newi2)
                  },
                  cloneProbability = cloneProbability)
            },
            operationExploration))(implicitly[Monad[M]])(selected)
        clamped = (bred: Vector[Individual]).map { (i: Individual) => i.copy(genome = i.genome.map { x: Double => max(0.0, min(1.0, x)) }) }
      } yield clamped
    }

  def elitism[M[_]: Monad: RandomGen](muByNiche: Int, niche: Niche[Individual, Int], historySize: Int): Objective[Individual, M] =
    byNicheO[Individual, Int, M](
      niche = niche,
      objective = (individuals: Vector[Individual]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[Individual, (V, Maybe[Int], Long), M](
            { (i: Individual) => (i.genome, i.operator, i.age) },
            clonesMergeHistories[Individual, Double, M](historySize))(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.exists { (_: Double).isNaN } }
          kept <- keepHighestRankedO[Individual, (Lazy[Int], Lazy[Double]), M](
            paretoRankingMinAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
            muByNiche)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
        } yield kept
    )

  def step[M[_]: Monad: RandomGen: Generational](
    fitness: Expression[(Random, Vector[Double]), Double],
    niche: Niche[Individual, Int],
    muByNiche: Int,
    lambda: Int,
    historySize: Int,
    cloneProbability: Double,
    operationExploration: Double): Vector[Individual] => M[Vector[Individual]] =
    stepEA[Individual, M, (Random, Individual)](
      { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
      withRandomGenB[Individual, M, Individual](breeding[M](lambda, niche, operationExploration, cloneProbability)),
      { case (rg: Random, i: Individual) => individualHistory.append(i, fitness((rg, i.genome))) },
      elitism[M](muByNiche, niche, historySize),
      muPlusLambda[Individual])

  def algorithm(muByNiche: Int, lambda: Int, genomeSize: Int, niche: Niche[Individual, Int], historySize: Int, operationExploration: Double, cloneProbability: Double) =
    new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Individual, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
      implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

      def initialGenomes: EvolutionState[Unit, Vector[Individual]] = NoisyProfile.initialPopulation[EvolutionStateMonad[Unit]#l](muByNiche, genomeSize)
      def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Individual] = NoisyProfile.breeding[EvolutionStateMonad[Unit]#l](lambda, niche, operationExploration, cloneProbability)
      def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NoisyProfile.elitism[EvolutionStateMonad[Unit]#l](muByNiche, niche, historySize)

      def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
      def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    }

}
