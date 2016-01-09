/*
 * Copyright (C) 16/12/2015 Guillaume Chérel
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
import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.contexts._
import fr.iscpif.mgo.expressions._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.elitism._
import GenomeVectorDouble._

import monocle.macros.{ Lenses, GenLens }
import scala.language.higherKinds

import scala.math._
import scala.util.Random

import scalaz._
import Scalaz._

import contexts.default._

object pse {

  case class PSE(
      lambda: Int,
      phenotype: Expression[Vector[Double], Vector[Double]],
      pattern: Vector[Double] => Vector[Int],
      genomeSize: Int,
      operatorExploration: Double) extends Algorithm[EvolutionState[HitMap, ?], Individual, Genome, EvolutionData[HitMap]] {

    def initialState(rng: Random) = EvolutionData[HitMap](random = rng, s = Map.empty)
    def initialGenomes = pse.initialGenomes(lambda, genomeSize)
    def breeding = pse.breeding(lambda, pattern, operatorExploration)
    def expression = pse.expression(phenotype)
    def elitism = pse.elitism(pattern)
    def step = deterministicStep[EvolutionState[HitMap, ?], Individual, Genome](breeding, expression, elitism)

    def run[A](x: EvolutionState[HitMap, A], s: EvolutionData[HitMap]): (EvolutionData[HitMap], A) = default.unwrap(x, s)
  }

  type V = Vector[Double]

  @Lenses case class Genome(values: V, operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, phenotype: Vector[Double], age: Long)

  type HitMap = Map[Vector[Int], Int]

  implicit def hitMapper: HitMapper[EvolutionState[HitMap, ?], Vector[Int]] =
    new HitMapper[EvolutionState[HitMap, ?], Vector[Int]] {
      def get: EvolutionState[HitMap, Map[Vector[Int], Int]] =
        evolutionStateMonadState[HitMap].get.map(_.s)

      def set(newMap: Map[Vector[Int], Int]): EvolutionState[HitMap, Unit] =
        evolutionStateMonadState[HitMap].modify { s => s.copy(s = newMap) }

      def hitCount(cell: Vector[Int]): EvolutionState[HitMap, Int] =
        evolutionStateMonadState[HitMap].get.map {
          _.s.getOrElse(cell, 0)
        }

      def hits(cells: Vector[Vector[Int]]): EvolutionState[HitMap, Unit] =
        evolutionStateMonadState[HitMap].modify { s =>
          val newS = s.s ++ (cells.map { c => (c, s.s.getOrElse(c, 0) + 1) })
          s.copy(s = newS)
        }
    }

  def patternGrid(lowBound: Vector[Double], highBound: Vector[Double], definition: Vector[Int])(value: Vector[Double]): Vector[Int] =
    (value zip definition zip lowBound zip highBound).map {
      case (((x, d), lb), hb) =>
        val step = (hb - lb) / d
        val p = ((x - lb) / step).floor.toInt
        max(0, min(d, p))
    }

  def buildIndividual(genome: Genome, phenotype: Vector[Double]) = Individual(genome, phenotype, 0)

  def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[HitMap, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[HitMap, ?], Genome](Genome.apply)(mu, genomeSize)

  def breeding(
    lambda: Int,
    pattern: Vector[Double] => Vector[Int],
    operatorExploration: Double) =
    pseOperations.breeding[EvolutionState[HitMap, ?], Individual, Genome](
      Individual.genome.get,
      Genome.values.get,
      Genome.operator.get,
      Individual.phenotype.get _ andThen pattern,
      Genome.apply
    )(lambda, operatorExploration)

  def elitism(pattern: Vector[Double] => Vector[Int]) =
    pseOperations.elitism[EvolutionState[HitMap, ?], Individual](
      (Individual.genome composeLens Genome.values).get,
      Individual.phenotype.get _ andThen pattern,
      Individual.age
    )

  def expression(phenotype: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    pseOperations.expression[Genome, Individual](Genome.values.get, buildIndividual)(phenotype)

  case class OpenMOLE(
    lambda: Int,
    pattern: Vector[Double] => Vector[Int],
    genomeSize: Int,
    operatorExploration: Double)

  object OpenMOLE {

    import fr.iscpif.mgo.contexts.default._

    implicit def integration: openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] {
      type M[A] = EvolutionState[HitMap, A]
      type G = Genome
      type I = Individual
      type S = EvolutionData[HitMap]

      def iManifest = implicitly
      def gManifest = implicitly
      def sManifest = implicitly
      def mMonad = implicitly
      def mGenerational = implicitly
      def mStartTime = implicitly

      def operations(om: OpenMOLE) = new Ops {
        def randomLens = GenLens[S](_.random)
        def generation(s: S) = s.generation
        def values(genome: G) = Genome.values.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = Individual.phenotype.get(individual)
        def buildIndividual(genome: G, phenotype: Vector[Double]) = Individual(genome, phenotype, 0)
        def initialState(rng: Random) = EvolutionData[HitMap](random = rng, s = Map())
        def initialGenomes(n: Int): M[Vector[G]] = pse.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[M, I, G] = pse.breeding(n, om.pattern, om.operatorExploration)
        def elitism: Elitism[M, I] = pse.elitism(om.pattern)
        def migrateToIsland(i: I): I = i
      }

      def unwrap[A](x: M[A], s: S): (S, A) = default.unwrap(x, s)
    }
  }

  //  /**
  //   * The default PSE algorithm working with Vector[Double] genomes and patterns. (TODO: Can we abstract this to
  //   * make a default PSE algorithm for any types for genomes and patterns?)
  //   */
  //  object Algorithm {
  //
  //    import fr.iscpif.mgo.Contexts.default._
  //
  //    type V = Vector[Double]
  //    case class Genome(values: V, operator: Maybe[Int], generation: Long)
  //    case class Individual(genome: Genome, pattern: Vector[Double])
  //    type HitMap = Map[Vector[Int], Int]
  //
  //    val iPattern: Lens[Individual, Vector[Double]] = Lens.lensu(
  //      set = (i, v) => i.copy(pattern = v),
  //      get = _.pattern
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
  //    // HitMapper instance of the default Context

  //
  //    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[HitMap, Vector[Genome]] =
  //      PSE.initialGenomes[EvolutionStateMonad[HitMap]#l, Genome](Genome)(mu, genomeSize)
  //    def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double, cell: Individual => Vector[Int]): Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] =
  //      PSE.breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome, Vector[Int]](
  //        iGenome, gValues, gOperator, Genome, cell
  //      )(lambda, operatorExploration, cloneProbability)
  //    def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
  //      PSE.expression[Genome, Individual](gValues, Individual)(fitness)
  //    def elitism(muPerCell: Int, cell: Individual => Vector[Int]): Elitism[EvolutionStateMonad[HitMap]#l, Individual] =
  //      PSE.elitism[EvolutionStateMonad[HitMap]#l, Individual, Vector[Int]](iGenomeValues, iGeneration, cell)(muPerCell)
  //
  //    def step(
  //      mu: Int,
  //      lambda: Int,
  //      fitness: Expression[Vector[Double], Vector[Double]],
  //      operatorExploration: Double,
  //      cloneProbability: Double,
  //      cell: Individual => Vector[Int]): Kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Vector[Individual]] =
  //      PSE.step[EvolutionStateMonad[HitMap]#l, Individual, Genome](
  //        breeding(lambda, operatorExploration, cloneProbability, cell),
  //        expression(fitness),
  //        elitism(mu, cell)
  //      )
  //
  //    def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = default.wrap[HitMap, A](x)
  //    def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = default.unwrap[HitMap, A](Map.empty[Vector[Int], Int])(x)
  //
  //    def apply(
  //      initialPopSize: Int,
  //      keepPerCell: Int,
  //      lambda: Int,
  //      express: Vector[Double] => Vector[Double],
  //      genomeSize: Int,
  //      operatorExploration: Double,
  //      cloneProbability: Double,
  //      anchor: Vector[Double],
  //      discretisationStep: Vector[Double],
  //      lowBound: Vector[Double],
  //      highBound: Vector[Double]) =
  //      new Algorithm[EvolutionStateMonad[HitMap]#l, Individual, Genome, ({ type l[x] = (EvolutionData[HitMap], x) })#l] {
  //
  //        val cell: Individual => Vector[Int] = PSE.Algorithm.cell(anchor, discretisationStep, lowBound, highBound)
  //
  //        def initialGenomes: EvolutionState[HitMap, Vector[Genome]] = PSE.Algorithm.initialGenomes(initialPopSize, genomeSize)
  //        def breeding: Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] = PSE.Algorithm.breeding(lambda, operatorExploration, cloneProbability, cell)
  //        def expression: Expression[Genome, Individual] = PSE.Algorithm.expression(express)
  //        def elitism: Elitism[EvolutionStateMonad[HitMap]#l, Individual] = PSE.Algorithm.elitism(keepPerCell, cell)
  //
  //        def step: Kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Vector[Individual]] = PSE.Algorithm.step(keepPerCell, lambda, express, operatorExploration, cloneProbability, cell)
  //
  //        def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = PSE.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = PSE.Algorithm.unwrap(x)
  //
  //      }
  //
  //    def algoOpenMOLE(initialPopSize: Int,
  //      keepPerCell: Int,
  //      genomeSize: Int,
  //      operatorExploration: Double,
  //      cloneProbability: Double,
  //      anchor: Vector[Double],
  //      discretisationStep: Vector[Double],
  //      lowBound: Vector[Double],
  //      highBound: Vector[Double]) =
  //      new AlgorithmOpenMOLE[EvolutionStateMonad[HitMap]#l, Individual, Genome, EvolutionData[HitMap]] {
  //
  //        val cell: Individual => Vector[Int] = PSE.Algorithm.cell(anchor, discretisationStep, lowBound, highBound)
  //
  //        val cRandom: Lens[EvolutionData[HitMap], Random] = Lens.lensu(
  //          set = (e, r) => e.copy(random = r),
  //          get = _.random
  //        )
  //
  //        def initialGenomes(n: Int): EvolutionState[HitMap, Vector[Genome]] = PSE.Algorithm.initialGenomes(n, genomeSize)
  //        def breeding(n: Int): Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] = PSE.Algorithm.breeding(n, operatorExploration, cloneProbability, cell)
  //        def elitism: Elitism[EvolutionStateMonad[HitMap]#l, Individual] = PSE.Algorithm.elitism(keepPerCell, cell)
  //
  //        def initForIsland(i: Individual): Individual = i
  //
  //        def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = PSE.Algorithm.wrap(x)
  //        def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = PSE.Algorithm.unwrap(x)
  //
  //      }
  //
  //  }

}

object pseOperations {

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      operatorExploration: Double)(implicit MH: HitMapper[M, Vector[Int]]): Breeding[M, I, G] =
    for {
      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
      gs <- tournament(reversedRanking[M, I](hitCountRanking[M, I, Vector[Int]](pattern)), lambda + 1) andThen
        pairConsecutive andThen
        mapPureB { case (g1, g2) => ((genome andThen genomeValues)(g1), (genome andThen genomeValues)(g2)) } andThen
        applyDynamicOperator(operatorStatistics, operatorExploration) andThen
        flatMapPureB { case ((g1, g2), op) => Vector((g1, op), (g2, op)) } andThen
        randomTakeLambda(lambda) andThen
        clamp(GenLens[(Vector[Double], Int)](_._1)) andThen
        mapPureB { case (g, op) => buildGenome(g, Maybe.just(op)) }
    } yield gs

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    values: I => Vector[Double],
    pattern: I => Vector[Int],
    age: monocle.Lens[I, Long])(implicit MH: HitMapper[M, Vector[Int]]): Elitism[M, I] =
    addHits[M, I, Vector[Int]](pattern, age.get) andThen
      applyCloneStrategy(values, keepYoungest[M, I](age.get)) andThen
      keepNiches(
        niche = pattern,
        objective = randomO[M, I](1)
      ) andThen incrementGeneration(age)

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Vector[Double]) => I)(express: Expression[Vector[Double], Vector[Double]]): Expression[G, I] =
    (g: G) => build(g, express(values(g)))
}