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
import monocle.macros.GenLens

import scala.language.higherKinds

import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.niche._

import scala.math._
import scala.util.Random
import scalaz._
import Scalaz._

object Profile {

  def initialGenomes[M[_], G](gCons: (Vector[Double], Maybe[Int], Long) => G)(mu: Int, genomeSize: Int)(
    implicit MM: Monad[M], MR: RandomGen[M]): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      genomes = values.map { (vs: Vector[Double]) => gCons(vs, Maybe.empty, 0) }
    } yield genomes

  def breeding[M[_], I, G](
    iFitness: Lens[I, Double],
    iGenome: Lens[I, G],
    gValues: Lens[G, Vector[Double]],
    gOperator: Lens[G, Maybe[Int]],
    gCons: (Vector[Double], Maybe[Int], Long) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double)(
        implicit MM: Monad[M], MR: RandomGen[M], MG: Generational[M]): Breeding[M, I, G] = {
    type V = Vector[Double]
    for {
      // Select Parents
      parents <- tournament[M, I, Lazy[Int]](
        ranking = profileRanking[M, I](niche, iFitness.get(_: I)),
        size = if (lambda % 2 == 0) lambda else lambda + 1,
        rounds = size => math.round(math.log10(size).toInt))
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
      offspringsAndOpsLambdaAdjusted <- thenK(randomTakeLambda[M, (Vector[Double], Int)](lambda))(offspringsAndOps)
      // Clamp genome values between 0 and 1
      clamped <- thenK(clamp[M, (Vector[Double], Int)](GenLens[(Vector[Double], Int)](_._1)))(offspringsAndOpsLambdaAdjusted)
      // Add the current generation to new offsprings
      offspringsOpsGens <- thenK(mapB[M, (Vector[Double], Int), (Vector[Double], Int, Long)] {
        case (g, op) => MG.getGeneration.>>=[(Vector[Double], Int, Long)] { gen: Long => (g, op, gen).point[M] }
      })(clamped)
      // Construct the final G type
      gs <- thenK(mapPureB[M, (Vector[Double], Int, Long), G] { case (g, op, gen) => gCons(g, Maybe.just(op), gen) })(offspringsOpsGens)
    } yield gs
  }

  def expression[G, I](
    gValues: Lens[G, Vector[Double]],
    iCons: (G, Double) => I)(
      fitness: Vector[Double] => Double): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues.get(g)))

  def elitism[M[_], I](
    iFitness: Lens[I, Double],
    iGenomeValues: Lens[I, Vector[Double]],
    iGeneration: Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int])(
      implicit MM: Monad[M], MR: RandomGen[M]): Objective[M, I] =
    for {
      // Declone
      decloned <- applyCloneStrategy[M, I, Vector[Double]](
        { (i: I) => iGenomeValues.get(i) },
        keepYoungest[M, I] { iGeneration })
      // Filter out NaNs
      noNaN = (decloned: Vector[I]).filterNot { iGenomeValues.get(_).exists { (_: Double).isNaN } }
      // Keep in each niche muByNiche individuals with lowest fitness
      kept <- thenK(
        byNicheO[M, I, Int](
          niche = niche,
          objective = minimiseO[M, I, Double]({ i: I => iFitness.get(i) }, muByNiche))
      )(noNaN)
    } yield kept

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

  object Algorithm {
    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], generation: Long)
    case class Individual(genome: Genome, fitness: Double)

    val iFitness: Lens[Individual, Double] = Lens.lensu(
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
      Profile.initialGenomes[EvolutionStateMonad[Unit]#l, Genome](Genome)(mu, genomeSize)
    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.breeding[EvolutionStateMonad[Unit]#l, Individual, Genome](
      iFitness, iGenome, gValues, gOperator, Genome
    )(lambda, niche, operatorExploration)
    def expression(fitness: Expression[Vector[Double], Double]): Expression[Genome, Individual] =
      Profile.expression[Genome, Individual](gValues, Individual)(fitness)
    def elitism(muByNiche: Int, niche: Niche[Individual, Int]): Objective[EvolutionStateMonad[Unit]#l, Individual] =
      Profile.elitism[EvolutionStateMonad[Unit]#l, Individual](iFitness, iGenomeValues, iGeneration)(muByNiche, niche)

    def step(
      muByNiche: Int,
      lambda: Int,
      fitness: Vector[Double] => Double,
      niche: Niche[Individual, Int],
      operatorExploration: Double): Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
      Profile.step[EvolutionStateMonad[Unit]#l, Individual, Genome](
        breeding(lambda, niche, operatorExploration),
        expression(fitness),
        elitism(muByNiche, niche)
      )

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(
      muByNiche: Int,
      lambda: Int,
      fitness: Vector[Double] => Double,
      niche: Niche[Individual, Int],
      genomeSize: Int,
      operatorExploration: Double) =
      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        //implicit lazy val m: Monad[EvolutionStateMonad[Unit]#l] = evolutionStateMonad

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(muByNiche, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.Algorithm.breeding(lambda, niche, operatorExploration)
        def expression: Expression[Genome, Individual] = Profile.Algorithm.expression(fitness)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = Profile.Algorithm.elitism(muByNiche, niche)

        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] = Profile.Algorithm.step(muByNiche, lambda, fitness, niche, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)

      }

    def algoOpenMOLE(muByNiche: Int, genomeSize: Int, operatorExploration: Double, x: Int, nX: Int) =
      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Genome, EvolutionData[Unit]] {

        //implicit lazy val m: Monad[EvolutionStateMonad[Unit]#l] = evolutionStateMonad

        val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
          set = (e, r) => e.copy(random = r),
          get = _.random
        )

        def niche(i: Individual): Int = genomeProfile[Individual](
          values = (iGenome >=> gValues).get,
          x = x,
          nX = nX)(i)

        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(n, genomeSize)
        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.Algorithm.breeding(n, niche, operatorExploration)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = Profile.Algorithm.elitism(muByNiche, niche)

        def initForIsland(i: Individual): Individual = i

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)

      }

  }
}
