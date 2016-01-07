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

import monocle.macros.{ Lenses, GenLens }

import scala.language.higherKinds

import fr.iscpif.mgo._
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

object Profile {

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    fitness: I => Double,
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double): Breeding[M, I, G] =
    for {
      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
      gs <- tournament(
        profileRanking[M, I](niche, fitness),
        lambda + 1,
        rounds = size => math.round(math.log10(size).toInt)
      ) andThen
        pairConsecutive andThen
        mapPureB { case (g1, g2) => ((genome andThen genomeValues)(g1), (genome andThen genomeValues)(g2)) } andThen
        applyDynamicOperator(operatorStatistics, operatorExploration) andThen
        flatMapPureB { case ((g1, g2), op) => Vector((g1, op), (g2, op)) } andThen
        randomTakeLambda(lambda) andThen
        clamp(GenLens[(Vector[Double], Int)](_._1)) andThen
        mapPureB { case (g, op) => buildGenome(g, Maybe.just(op)) }
    } yield gs

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Double, Long) => I)(fitness: Vector[Double] => Double): Expression[G, I] =
    (g: G) => build(g, fitness(values(g)), 0)

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    fitness: I => Double,
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int]): Elitism[M, I] =
    applyCloneStrategy(values, keepYoungest[M, I](age.get)) andThen
      filterNaN(values) andThen
      keepNiches(
        niche = niche,
        objective = minimiseO[M, I, Double](fitness, muByNiche)
      ) andThen incrementGeneration(age)

  def step[M[_]: Monad: RandomGen: Generational, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = deterministicStep(breeding, expression, elitism)

  /** The default NSGA2 algorithm */
  object Algorithm {

    import fr.iscpif.mgo.contexts.default._

    @Lenses case class Genome(values: Vector[Double], operator: Maybe[Int])
    @Lenses case class Individual(genome: Genome, fitness: Double, age: Long)

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
      GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](Genome.apply)(mu, genomeSize)

    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
      Profile.breeding[EvolutionState[Unit, ?], Individual, Genome](
        Individual.fitness.get, Individual.genome.get, Genome.values.get, Genome.operator.get, Genome.apply
      )(lambda, niche, operatorExploration)

    def expression(fitness: Expression[Vector[Double], Double]): Expression[Genome, Individual] =
      Profile.expression[Genome, Individual](Genome.values.get, Individual.apply)(fitness)

    def elitism(muByNiche: Int, niche: Niche[Individual, Int]): Elitism[EvolutionState[Unit, ?], Individual] =
      Profile.elitism[EvolutionState[Unit, ?], Individual](
        Individual.fitness.get,
        (Individual.genome composeLens Genome.values).get,
        Individual.age)(muByNiche, niche)

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(muByNiche: Int, lambda: Int, fitness: Vector[Double] => Double, niche: Niche[Individual, Int], genomeSize: Int, operatorExploration: Double) =
      new Algorithm[EvolutionState[Unit, ?], Individual, Genome, (EvolutionData[Unit], ?)] {
        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(muByNiche, genomeSize)
        def breeding: Breeding[EvolutionState[Unit, ?], Individual, Genome] = Profile.Algorithm.breeding(lambda, niche, operatorExploration)
        def expression: Expression[Genome, Individual] = Profile.Algorithm.expression(fitness)
        def elitism: Elitism[EvolutionState[Unit, ?], Individual] = Profile.Algorithm.elitism(muByNiche, niche)

        def step: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
          Profile.step[EvolutionState[Unit, ?], Individual, Genome](breeding, expression, elitism)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)
      }
  }

  //  object Algorithm {
  //    type V = Vector[Double]
  //    case class Genome(values: V, operator: Maybe[Int], generation: Long)
  //    case class Individual(genome: Genome, fitness: Double)
  //
  //    val iFitness: Lens[Individual, Double] = Lens.lensu(
  //      set = (i, v) => i.copy(fitness = v),
  //      get = _.fitness
  //    )
  //    val iGenome: Lens[Individual, Genome] = Lens.lensu(
  //      set = (i, g) => i.copy(genome = g),
  //      get = _.genome
  //    )
  //    val gValues: Lens[Genome, Vector[Double]] = Lens.lensu(
  //      set = (g, v) => g.copy(values = v),
  //      get = _.values
  //    )
  //    val gOperator: Lens[Genome, Maybe[Int]] = Lens.lensu(
  //      set = (g, o) => g.copy(operator = o),
  //      get = _.operator
  //    )
  //    val gGeneration: Lens[Genome, Long] = Lens.lensu(
  //      set = (g, e) => g.copy(generation = e),
  //      get = _.generation
  //    )
  //    val iGenomeValues: Lens[Individual, Vector[Double]] = iGenome >=> gValues
  //    val iGeneration: Lens[Individual, Long] = iGenome >=> gGeneration
  //
  //    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
  //      Profile.initialGenomes[EvolutionStateMonad[Unit]#l, Genome](Genome)(mu, genomeSize)
  //    def breeding(lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.breeding[EvolutionStateMonad[Unit]#l, Individual, Genome](
  //      iFitness, iGenome, gValues, gOperator, Genome
  //    )(lambda, niche, operatorExploration)
  //    def expression(fitness: Expression[Vector[Double], Double]): Expression[Genome, Individual] =
  //      Profile.expression[Genome, Individual](gValues, Individual)(fitness)
  //    def elitism(muByNiche: Int, niche: Niche[Individual, Int]): Elitism[EvolutionStateMonad[Unit]#l, Individual] =
  //      Profile.elitism[EvolutionStateMonad[Unit]#l, Individual](iFitness, iGenomeValues, iGeneration)(muByNiche, niche)
  //
  //    def step(
  //      muByNiche: Int,
  //      lambda: Int,
  //      fitness: Vector[Double] => Double,
  //      niche: Niche[Individual, Int],
  //      operatorExploration: Double): Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] =
  //      Profile.step[EvolutionStateMonad[Unit]#l, Individual, Genome](
  //        breeding(lambda, niche, operatorExploration),
  //        expression(fitness),
  //        elitism(muByNiche, niche)
  //      )
  //
  //    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
  //    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)
  //
  //    def apply(
  //      muByNiche: Int,
  //      lambda: Int,
  //      fitness: Vector[Double] => Double,
  //      niche: Niche[Individual, Int],
  //      genomeSize: Int,
  //      operatorExploration: Double) =
  //      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
  //
  //        //implicit lazy val m: Monad[EvolutionStateMonad[Unit]#l] = evolutionStateMonad
  //
  //        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(muByNiche, genomeSize)
  //        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.Algorithm.breeding(lambda, niche, operatorExploration)
  //        def expression: Expression[Genome, Individual] = Profile.Algorithm.expression(fitness)
  //        def elitism: Elitism[EvolutionStateMonad[Unit]#l, Individual] = Profile.Algorithm.elitism(muByNiche, niche)
  //
  //        def step: Kleisli[EvolutionStateMonad[Unit]#l, Vector[Individual], Vector[Individual]] = Profile.Algorithm.step(muByNiche, lambda, fitness, niche, operatorExploration)
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)
  //
  //      }
  //
  //    def algoOpenMOLE(muByNiche: Int, genomeSize: Int, operatorExploration: Double, x: Int, nX: Int) =
  //      new AlgorithmOpenMOLE[EvolutionStateMonad[Unit]#l, Individual, Genome, EvolutionData[Unit]] {
  //
  //        //implicit lazy val m: Monad[EvolutionStateMonad[Unit]#l] = evolutionStateMonad
  //
  //        val cRandom: Lens[EvolutionData[Unit], Random] = Lens.lensu(
  //          set = (e, r) => e.copy(random = r),
  //          get = _.random
  //        )
  //
  //        def niche(i: Individual): Int = genomeProfile[Individual](
  //          values = (iGenome >=> gValues).get,
  //          x = x,
  //          nX = nX)(i)
  //
  //        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(n, genomeSize)
  //        def breeding(n: Int): Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.Algorithm.breeding(n, niche, operatorExploration)
  //        def elitism: Elitism[EvolutionStateMonad[Unit]#l, Individual] = Profile.Algorithm.elitism(muByNiche, niche)
  //
  //        def initForIsland(i: Individual): Individual = i
  //
  //        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)
  //
  //      }
  //
  //  }
}
