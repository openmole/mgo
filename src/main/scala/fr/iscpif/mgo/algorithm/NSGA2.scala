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

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._

import scala.math._

import scalaz._
import Scalaz._

object NSGA2 {

  type V = Vector[Double]
  case class Genome(values: V, operator: Maybe[Int], generation: Long)
  case class Individual(genome: Genome, fitness: Vector[Double])

  def initialGenomes[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Genome]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      genomes = values.map { (vs: Vector[Double]) => Genome(vs, Maybe.empty, 0) }
    } yield genomes

  def breeding[M[_]: Monad: RandomGen: Generational](
    lambda: Int,
    operationExploration: Double = 0.1): Breeding[Individual, M, Genome] =
    (individuals: Vector[Individual]) => {

      for {
        rg <- implicitly[RandomGen[M]].split
        generation <- implicitly[Generational[M]].getGeneration
        selected <- tournament[Individual, (Lazy[Int], Lazy[Double]), M](
          paretoRankingAndCrowdingDiversity[Individual] { (i: Individual) => i.fitness }(rg),
          lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
        bred <- asB[Individual, (V, Maybe[Int]), M, (V, Maybe[Int]), Genome](
          { (i: Individual) => (i.genome.values, i.genome.operator) },
          { case (values: V, op: Maybe[Int]) => Genome(values, op, generation) },
          dynamicallyOpB[V, M, V, (V, V), (V, V)](
            pairConsecutive[V, M],
            { case (g1, g2) => Vector(g1, g2).point[M] },
            dynamicOperators.crossoversAndMutations[M],
            operationExploration))(implicitly[Monad[M]])(selected)
        clamped = (bred: Vector[Genome]).map { (g: Genome) => g.copy(values = g.values.map { x: Double => max(0.0, min(1.0, x)) }) }
      } yield clamped
    }

  def elitism[M[_]: Monad: RandomGen](mu: Int): Objective[Individual, M] =
    (individuals: Vector[Individual]) =>
      for {
        rg <- implicitly[RandomGen[M]].split
        decloned <- applyCloneStrategy[Individual, Genome, M](
          { (i: Individual) => i.genome },
          clonesKeepYoungest[Individual, M] { (i: Individual) => i.genome.generation })(implicitly[Monad[M]])(individuals)
        noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.values.exists { (_: Double).isNaN } }
        kept <- keepHighestRankedO[Individual, (Lazy[Int], Lazy[Double]), M](
          paretoRankingAndCrowdingDiversity[Individual] { (i: Individual) => i.fitness }(rg),
          mu)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
      } yield kept

  def step[M[_]: Monad: RandomGen: Generational](
    fitness: Genome => Vector[Double],
    mu: Int,
    lambda: Int): Vector[Individual] => M[Vector[Individual]] =
    stepEA[Individual, M, Genome](
      { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
      breeding[M](lambda),
      { (g: Genome) => Individual(g, fitness(g)) },
      elitism[M](mu),
      muPlusLambda[Individual])

  def algorithm(mu: Int, lambda: Int, genomeSize: Int) =
    new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
      implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

      def initialGenomes: EvolutionState[Unit, Vector[Genome]] = NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l](mu, genomeSize)
      def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Genome] = NSGA2.breeding[EvolutionStateMonad[Unit]#l](lambda)
      def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NSGA2.elitism[EvolutionStateMonad[Unit]#l](mu)

      def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
      def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    }
}