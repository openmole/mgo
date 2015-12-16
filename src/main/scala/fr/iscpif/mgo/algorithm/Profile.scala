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
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.niche._

import scala.math._
import scalaz._
import Scalaz._

object Profile {

  type V = Vector[Double]
  case class Genome(values: V, operator: Maybe[Int], generation: Long)
  case class Individual(genome: Genome, fitness: Double)

  def initialGenomes[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Genome]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      genomes = values.map { (vs: Vector[Double]) => Genome(vs, Maybe.empty, 0) }
    } yield genomes

  def breeding[M[_]: Monad: RandomGen: Generational](
    lambda: Int,
    niche: Niche[Individual, Int],
    operationExploration: Double): Breeding[Individual, M, Genome] =
    (individuals: Vector[Individual]) => {
      for {
        rg <- implicitly[RandomGen[M]].split
        generation <- implicitly[Generational[M]].getGeneration
        selected <- tournament[Individual, Lazy[Int], M](
          ranking = profileRanking(niche, (_: Individual).fitness),
          size = lambda,
          rounds = size => math.round(math.log10(size).toInt))(implicitly[Order[Lazy[Int]]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
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

  def elitism[M[_]: Monad: RandomGen](muByNiche: Int, niche: Niche[Individual, Int]): Objective[Individual, M] =
    byNicheO[Individual, Int, M](
      niche = niche,
      objective = (individuals: Vector[Individual]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[Individual, Genome, M](
            { (i: Individual) => i.genome },
            clonesKeepYoungest[Individual, M] { (i: Individual) => i.genome.generation })(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.values.exists { (_: Double).isNaN } }
          kept <- minimiseO[Individual, M, Double](
            { i: Individual => i.fitness },
            muByNiche)(implicitly[Monad[M]], implicitly[Order[Double]])(noNaN)
        } yield kept
    )

  def step[M[_]: Monad: RandomGen: Generational](
    fitness: Genome => Double,
    niche: Niche[Individual, Int],
    muByNiche: Int,
    lambda: Int,
    operationExploration: Double): Vector[Individual] => M[Vector[Individual]] =
    stepEA[Individual, M, Genome](
      { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
      breeding[M](lambda, niche, operationExploration),
      { (g: Genome) => Individual(g, fitness(g)) },
      elitism[M](muByNiche, niche),
      muPlusLambda[Individual])

  def algorithm(muByNiche: Int, lambda: Int, genomeSize: Int, niche: Niche[Individual, Int], operationExploration: Double = 0.1) =
    new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
      implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

      def initialGenomes: EvolutionState[Unit, Vector[Genome]] = Profile.initialGenomes[EvolutionStateMonad[Unit]#l](muByNiche, genomeSize)
      def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Genome] = Profile.breeding[EvolutionStateMonad[Unit]#l](lambda, niche, operationExploration)
      def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = Profile.elitism[EvolutionStateMonad[Unit]#l](muByNiche, niche)

      def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
      def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    }
}
