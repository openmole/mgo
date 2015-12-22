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
    iHistory: Lens[I, Vector[Double]])(i: I): Vector[Double] = Vector(iHistory.get(i).sum / iHistory.get(i).size, 1.0 / iHistory.get(i).size)

  def initialGenomes[M[_]: Monad: RandomGen, I](
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Double]) => I)(mu: Int, genomeSize: Int): M[Vector[(Random, I)]] =
    for {
      rgs <- implicitly[RandomGen[M]].split.replicateM(mu)
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = rgs.toVector zip values.map { (vs: Vector[Double]) => iCons(vs, Maybe.empty, 1, Vector.empty) }
    } yield indivs

  def breeding[M[_]: Monad: RandomGen: Generational, I](
    iHistory: Lens[I, Vector[Double]],
    iValues: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long],
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Double]) => I)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, (Random, I)] =
    for {
      // Select parents with the double objective of minimising fitness and maximising history size with a pareto ranking, and then maximising diversity
      parents <- tournament[M, I, (Lazy[Int], Lazy[Double])](
        ranking = paretoRankingMinAndCrowdingDiversity[M, I] { fitnessWithReplications(iHistory) },
        size = lambda,
        rounds = size => math.round(math.log10(size).toInt))
      // Compute the proportion of each operator in the population
      opstats = parents.map { iOperator.get }.collect { case Maybe.Just(op) => op }.groupBy(identity).mapValues(_.length.toDouble / parents.size)
      // Get the genome values
      parentgenomes <- thenK(mapPureB[M, I, Vector[Double]] { (iValues).get })(parents)
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
      // Clamp genome values between 0 and 1
      clamped <- thenK(mapPureB[M, (Vector[Double], Int), (Vector[Double], Int)] {
        Lens.firstLens[Vector[Double], Int] =>= { _ map { x: Double => max(0.0, min(1.0, x)) } }
      })(offspringsAndOps)
      // Construct the final I type
      is <- thenK(mapPureB[M, (Vector[Double], Int), I] { case (g, op) => iCons(g, Maybe.just(op), 0.toLong, Vector.empty) })(clamped)
      // Replace some offsprings by clones from the original population.
      // TODO: les clones sont tirés aléatoirement de la population initiale, pas de tirage par tournoi. Est-ce qu'il
      // faut biaiser le choix des clones par meilleure fitness et historique plus court?
      withclones <- clonesReplace[M, I, I](Kleisli.kleisli[M, I, I] { (i: I) => iAge.mod({ _ + 1 }, i).point[M] }, cloneProbability)(is)
      // Add an independant random number generator to each individual
      result <- thenK(withRandomGenB[M, I])(withclones)
    } yield result

  def expression[I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Double]])(fitness: (Random, Vector[Double]) => Double): Expression[(Random, I), I] =
    { case (rg, i) => iHistory.mod(_ :+ fitness(rg, iValues.get(i)), i) }

  def elitism[M[_]: Monad: RandomGen, I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int], historySize: Int): Objective[M, I] =
    for {
      // Declone
      decloned <- applyCloneStrategy[M, I, Vector[Double]](
        { (i: I) => iValues.get(i) },
        mergeHistories[M, I, Double](iAge, iHistory)(historySize))
      // Filter out NaNs
      noNaN = (decloned: Vector[I]).filterNot { iValues.get(_).exists { (_: Double).isNaN } }
      // Keep in each niche muByNiche individuals with lowest fitness
      kept <- thenK(
        byNicheO[M, I, Int](
          niche = niche,
          objective = keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](
            paretoRankingMinAndCrowdingDiversity[M, I] { fitnessWithReplications(iHistory) },
            muByNiche))
      )(noNaN)
    } yield kept

  def step[M[_]: Monad: RandomGen: Generational, I](
    breeding: Breeding[M, I, (Random, I)],
    expression: Expression[(Random, I), I],
    elitism: Objective[M, I]): Kleisli[M, Vector[I], Vector[I]] =
    stepEA[M, I, (Random, I)](
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

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[(Random, Individual)]] = NoisyProfile.initialGenomes[EvolutionStateMonad[Unit]#l, Individual](Individual)(mu, genomeSize)
    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] =
      NoisyProfile.breeding[EvolutionStateMonad[Unit]#l, Individual](
        iHistory, iValues, iOperator, iAge, Individual
      )(lambda, niche, operatorExploration, cloneProbability)
    def expression(fitness: (Random, Vector[Double]) => Double): Expression[(Random, Individual), Individual] =
      NoisyProfile.expression[Individual](iValues, iHistory)(fitness)
    def elitism(muByNiche: Int, niche: Niche[Individual, Int], historySize: Int): Objective[EvolutionStateMonad[Unit]#l, Individual] =
      NoisyProfile.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iHistory, iOperator, iAge)(muByNiche, niche, historySize)

    def step(
      muByNiche: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Double,
      niche: Niche[Individual, Int],
      historySize: Int,
      cloneProbability: Double,
      operatorExploration: Double): Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
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
      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] = NoisyProfile.Algorithm.initialGenomes(muByNiche, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] = NoisyProfile.Algorithm.breeding(lambda, niche, operatorExploration, cloneProbability)
        def expression: Expression[(Random, Individual), Individual] = NoisyProfile.Algorithm.expression(fitness)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyProfile.Algorithm.elitism(muByNiche, niche, historySize)

        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] = NoisyProfile.Algorithm.step(muByNiche, lambda, fitness, niche, historySize, cloneProbability, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyProfile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyProfile.Algorithm.unwrap(x)
      }
  }
}
