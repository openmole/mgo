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

import fr.iscpif.mgo._
import monocle.macros.{ Lenses, GenLens }
import ranking._
import niche._
import contexts._
import elitism._
import expressions._
import breeding._
import GenomeVectorDouble._

import scala.math._
import scala.util.Random
import scalaz._
import Scalaz._

import scala.language.higherKinds

object NoisyProfile {

  def aggregatedFitness[I](fitness: I => Vector[Double], aggregation: Vector[Double] => Double)(i: I): Vector[Double] =
    Vector(aggregation(fitness(i)), 1.0 / fitness(i).size.toDouble)

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    history: I => Vector[Double],
    aggregation: Vector[Double] => Double,
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
      gs <- tournament(
        ranking = paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history, aggregation)),
        size = lambda + 1,
        rounds = size => math.round(math.log10(size).toInt)
      ) andThen
        pairConsecutive andThen
        mapPureB { case (g1, g2) => ((genome andThen genomeValues)(g1), (genome andThen genomeValues)(g2)) } andThen
        applyDynamicOperator(operatorStatistics, operatorExploration) andThen
        flatMapPureB { case ((g1, g2), op) => Vector((g1, op), (g2, op)) } andThen
        randomTakeLambda(lambda) andThen
        clamp(GenLens[(Vector[Double], Int)](_._1)) andThen
        mapPureB { case (g, op) => buildGenome(g, Maybe.just(op)) } andThen
        clonesReplace(cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    history: monocle.Lens[I, Vector[Double]],
    aggregation: Vector[Double] => Double,
    values: I => Vector[Double],
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int], historySize: Int): Elitism[M, I] =
    applyCloneStrategy(values, mergeHistories[M, I, Double](historyAge, history)(historySize)) andThen
      filterNaN(values) andThen
      keepNiches[M, I, Int](
        niche = niche,
        objective = keepHighestRanked(
          paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history.get, aggregation)),
          muByNiche)
      ) andThen incrementGeneration(age)

  def expression[G, I](
    values: G => Vector[Double],
    builder: (G, Double) => I)(fitness: (Random, Vector[Double]) => Double): Expression[(Random, G), I] = {
    case (rg, g) => builder(g, fitness(rg, values(g)))
  }

  def step[M[_]: Monad: RandomGen: Generational: ParallelRandomGen, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)

  /** The default Profile algorithm */
  object Algorithm {

    import fr.iscpif.mgo.contexts.default._

    @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
    @Lenses case class Individual(genome: Genome, historyAge: Long, fitnessHistory: Vector[Double], age: Long)

    def buildIndividual(g: Genome, f: Double) = Individual(g, 1, Vector(f), 0)

    def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
      GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](Genome.apply)(lambda, genomeSize)

    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Double] => Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
      NoisyProfile.breeding[EvolutionState[Unit, ?], Individual, Genome](
        Individual.fitnessHistory.get, aggregation, Individual.genome.get, Genome.values.get, Genome.operator.get, Genome.apply
      )(lambda = lambda, niche = niche, operatorExploration = operatorExploration, cloneProbability = cloneProbability)

    def expression(fitness: (Random, Vector[Double]) => Double): Expression[(Random, Genome), Individual] =
      NoisyProfile.expression[Genome, Individual](Genome.values.get, buildIndividual)(fitness)

    def elitism(muByNiche: Int, niche: Niche[Individual, Int], historySize: Int, aggregation: Vector[Double] => Double): Elitism[EvolutionState[Unit, ?], Individual] =
      NoisyProfile.elitism[EvolutionState[Unit, ?], Individual](
        Individual.fitnessHistory,
        aggregation,
        (Individual.genome composeLens Genome.values).get,
        Individual.age,
        Individual.historyAge
      )(muByNiche, niche, historySize)

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(
      muByNiche: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Double,
      aggregation: Vector[Double] => Double,
      niche: Niche[Individual, Int],
      genomeSize: Int,
      historySize: Int = 100,
      cloneProbability: Double = 0.2,
      operatorExploration: Double = 0.1) =
      new Algorithm[EvolutionState[Unit, ?], Individual, Genome, (EvolutionData[Unit], ?)] {
        def initialGenomes = NoisyProfile.Algorithm.initialGenomes(lambda, genomeSize)

        def breeding =
          NoisyProfile.Algorithm.breeding(
            lambda = lambda,
            niche = niche,
            operatorExploration = operatorExploration,
            cloneProbability = cloneProbability,
            aggregation = aggregation
          )

        def expression = NoisyProfile.Algorithm.expression(fitness)

        def elitism =
          NoisyProfile.Algorithm.elitism(
            muByNiche = muByNiche,
            niche = niche,
            historySize = historySize,
            aggregation = aggregation
          )

        def step =
          NoisyProfile.step[EvolutionState[Unit, ?], Individual, Genome](breeding, expression, elitism)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)
      }
  }

  //  object Algorithm {
  //
  //    case class Individual(
  //      genome: Vector[Double],
  //      operator: Maybe[Int],
  //      age: Long,
  //      fitnessHistory: Vector[Double])
  //
  //    val iValues: Lens[Individual, Vector[Double]] = Lens.lensu(
  //      set = (i, v) => i.copy(genome = v),
  //      get = _.genome
  //    )
  //    val iOperator: Lens[Individual, Maybe[Int]] = Lens.lensu(
  //      set = (i, o) => i.copy(operator = o),
  //      get = _.operator
  //    )
  //    val iAge: Lens[Individual, Long] = Lens.lensu(
  //      set = (i, a) => i.copy(age = a),
  //      get = _.age
  //    )
  //    val iHistory: Lens[Individual, Vector[Double]] = Lens.lensu(
  //      set = (i, h) => i.copy(fitnessHistory = h),
  //      get = _.fitnessHistory
  //    )
  //
  //    implicit val individualHistory = new History[Double, Individual] {
  //      val lens = iHistory
  //    }
  //
  //    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Individual]] = NoisyProfile.initialGenomes[EvolutionStateMonad[Unit]#l, Individual](Individual)(mu, genomeSize)
  //    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] =
  //      NoisyProfile.breeding[EvolutionStateMonad[Unit]#l, Individual](
  //        iHistory, iValues, iOperator, iAge, Individual
  //      )(lambda, niche, operatorExploration, cloneProbability)
  //    def expression(fitness: (Random, Vector[Double]) => Double): Expression[(Random, Individual), Individual] =
  //      NoisyProfile.expression[Individual](iValues, iHistory)(fitness)
  //    def elitism(muByNiche: Int, niche: Niche[Individual, Int], historySize: Int): Objective[EvolutionStateMonad[Unit]#l, Individual] =
  //      NoisyProfile.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iHistory, iOperator, iAge)(muByNiche, niche, historySize)
  //
  //    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
  //    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)
  //
  //    def apply(
  //      muByNiche: Int,
  //      lambda: Int,
  //      fitness: (Random, Vector[Double]) => Double,
  //      niche: Niche[Individual, Int],
  //      genomeSize: Int,
  //      historySize: Int,
  //      operatorExploration: Double,
  //      cloneProbability: Double) =
  //      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {
  //
  //        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] =
  //          for {
  //            ig <- NoisyProfile.Algorithm.initialGenomes(muByNiche, genomeSize)
  //            //Add an independant random number generator to each individual
  //            result <- withRandomGenB[EvolutionStateMonad[Unit]#l, Individual].run(ig)
  //          } yield result
  //
  //        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] =
  //          for {
  //            bred <- NoisyProfile.Algorithm.breeding(lambda, niche, operatorExploration, cloneProbability)
  //            //Add an independant random number generator to each individual
  //            result <- thenK[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual], Vector[(Random, Individual)]](withRandomGenB[EvolutionStateMonad[Unit]#l, Individual])(bred)
  //          } yield result
  //
  //        def expression: Expression[(Random, Individual), Individual] = NoisyProfile.Algorithm.expression(fitness)
  //
  //        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyProfile.Algorithm.elitism(muByNiche, niche, historySize)
  //
  //        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
  //          NoisyProfile.step[EvolutionStateMonad[Unit]#l, Individual](
  //            breeding,
  //            expression,
  //            elitism)
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyProfile.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyProfile.Algorithm.unwrap(x)
  //      }
  //
  //    def algoOpenMOLE(muByNiche: Int, operatorExploration: Double, genomeSize: Int, historySize: Int, cloneProbability: Double, x: Int, nX: Int) =
  //      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Individual, EvolutionData[Unit]] {
  //
  //        val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
  //          set = (e, r) => e.copy(random = r),
  //          get = _.random
  //        )
  //
  //        def niche(i: Individual): Int = genomeProfile[Individual](
  //          values = iValues.get,
  //          x = x,
  //          nX = nX)(i)
  //
  //        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Individual]] = NoisyProfile.Algorithm.initialGenomes(n, genomeSize)
  //        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Individual] = NoisyProfile.Algorithm.breeding(n, niche, operatorExploration, cloneProbability)
  //        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyProfile.Algorithm.elitism(muByNiche, niche, historySize)
  //
  //        def initForIsland(i: Individual): Individual = i.copy(age = 0)
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyProfile.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyProfile.Algorithm.unwrap(x)
  //
  //      }
  //  }
}
