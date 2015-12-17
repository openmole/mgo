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

object NoisyNSGA2 {

  type V = Vector[Double]
  case class Individual(
    genome: V,
    operator: Maybe[Int],
    age: Long,
    fitnessHistory: Vector[Vector[Double]])

  implicit val individualAge: Age[Individual] = new Age[Individual] {
    def getAge(i: Individual): Long = i.age
    def setAge(i: Individual, a: Long): Individual = i.copy(age = a)
  }

  implicit val individualHistory: PhenotypeHistory[Individual, Vector[Double]] = new PhenotypeHistory[Individual, Vector[Double]] {
    def getPhenotype(i: Individual): Vector[Double] = i.fitnessHistory.last
    def append(i: Individual, p: Vector[Double]): Individual = i.copy(fitnessHistory = i.fitnessHistory :+ p)
    def getHistory(i: Individual): Vector[Vector[Double]] = i.fitnessHistory
    def setHistory(i: Individual, h: Vector[Vector[Double]]): Individual = i.copy(fitnessHistory = h)
  }

  def fitnessWithReplications(i: Individual): Vector[Double] = individualHistory.getPhenotype(i) ++ Vector(1.0 / individualHistory.getHistory(i).size.toDouble)

  def initialPopulation[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Individual]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = values.map { vs: Vector[Double] => Individual(genome = vs, operator = Maybe.empty, age = 1, fitnessHistory = Vector.empty) }
    } yield indivs

  def breeding[M[_]: Monad: RandomGen: Generational](
    lambda: Int,
    operationExploration: Double,
    cloneProbability: Double): Breeding[Individual, M, Individual] =
    (individuals: Vector[Individual]) => {

      //TODO: Vérifier le sens de paretoRanking
      for {
        rg <- implicitly[RandomGen[M]].split
        selected <- tournament[Individual, (Lazy[Int], Lazy[Double]), M](
          paretoRankingMinAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
          lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
        // Ce qui suit est compliqué parce qu'il y a plusieurs étapes de fonctions imbriquées: On utilise liftB pour
        // transformer un Individual en (Individual, Operateur) pour correspondre à la signature de dynamicallyOpB. Puis, on
        // associe les individus par paires parceque les operateurs qu'on utilise ensuite (crossover + mutation) prennent des
        // paires et renvoiens des paires. Ensuite, on map sur les operateurs crossoversAndMutations pour les transformer en operateurs
        // qui prennent et renvoient des paires d'individus plutôt que de Vecteur[Double]. L'avantage de cette approche est que les fonctions individuelles
        // sont minimalistes, et aussi que la signature des fonction nous donne beaucoup d'info sur ce que fait la fonction, le
        // désavantage est qu'il y a beaucoup de travail pour les combiner ensemble. On pourrait peut-être gagner en simplicité sur
        // cette transformation de signatures avec une autre approche: par exemple, si toutes les fonctions de breeding prennent des fonctions
        // qui leur permettent d'extraire les info dont elles ont besoin à partir des individus (ce que fait actuellement liftB), ou des lenses.
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

  def elitism[M[_]: Monad: RandomGen](mu: Int, historySize: Int): Objective[Individual, M] =
    (individuals: Vector[Individual]) =>
      for {
        rg <- implicitly[RandomGen[M]].split
        decloned <- applyCloneStrategy[Individual, (V, Maybe[Int], Long), M](
          { (i: Individual) => (i.genome, i.operator, i.age) },
          clonesMergeHistories[Individual, V, M](historySize))(implicitly[Monad[M]])(individuals)
        noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.exists { (_: Double).isNaN } }
        kept <- keepHighestRankedO[Individual, (Lazy[Int], Lazy[Double]), M](
          paretoRankingMinAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
          mu)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
      } yield kept

  def step[M[_]: Monad: RandomGen: Generational](
    fitness: Expression[(Random, Vector[Double]), Vector[Double]],
    mu: Int,
    lambda: Int,
    historySize: Int,
    operationExploration: Double,
    cloneProbability: Double): Vector[Individual] => M[Vector[Individual]] =
    stepEA[Individual, M, (Random, Individual)](
      { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
      withRandomGenB[Individual, M, Individual](breeding[M](lambda, operationExploration, cloneProbability)),
      { case (rg: Random, i: Individual) => individualHistory.append(i, fitness((rg, i.genome))) },
      elitism[M](mu, historySize),
      muPlusLambda[Individual])

  /*def algorithm(
    mu: Int,
    lambda: Int,
    genomeSize: Int,
    historySize: Int,
    operationExploration: Double,
    cloneProbability: Double) =
    new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Individual, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
      implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

      def initialGenomes: EvolutionState[Unit, Vector[Individual]] = NoisyNSGA2.initialPopulation[EvolutionStateMonad[Unit]#l](mu, genomeSize)
      def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.breeding[EvolutionStateMonad[Unit]#l](lambda, operationExploration, cloneProbability)
      def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NoisyNSGA2.elitism[EvolutionStateMonad[Unit]#l](mu, historySize)

      def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
      def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)
    }*/
}

