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

import scala.language.higherKinds

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._

import scala.math._
import scala.util.Random

import scalaz._
import Scalaz._
import scalaz.effect.IO

object PSE {

  def initialGenomes[M[_]: Monad: RandomGen, G](cons: (Vector[Double], Maybe[Int], Long) => G)(mu: Int, genomeSize: Int): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      gs = values.map { (vs: Vector[Double]) => cons(vs, Maybe.empty, 0) }
    } yield gs

  def breeding[M[_]: Monad: RandomGen: Generational: ({ type l[x[_]] = HitMapper[x, C] })#l, I, G, C](
    iGenome: Lens[I, G],
    gValues: Lens[G, Vector[Double]],
    gOperator: Lens[G, Maybe[Int]],
    gCons: (Vector[Double], Maybe[Int], Long) => G,
    cell: I => C)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] = {
    for {
      // Select lambda parents whose corresponding cell has low hit count
      parents <- tournament[M, I, Lazy[Int]](
        ranking = reversedRanking[M, I](hitCountRanking[M, I, C](cell)),
        size = lambda)
      // Compute the proportion of each operator in the population
      opstats = parents.map { (iGenome >=> gOperator).get }.collect { case Maybe.Just(op) => op }.groupBy(identity).mapValues(_.length.toDouble / parents.size)
      // Get the genome values
      parentgenomes <- thenK(mapPureB[M, I, Vector[Double]] { (iGenome >=> gValues).get })(parents)
      // Pair parents together
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
      // Add the current generation to new offsprings
      offspringsOpsGens <- thenK(mapB[M, (Vector[Double], Int), (Vector[Double], Int, Long)] {
        case (g, op) => implicitly[Generational[M]].getGeneration.>>=[(Vector[Double], Int, Long)] { gen: Long => (g, op, gen).point[M] }
      })(clamped)
      // Construct the final G type
      gs <- thenK(mapPureB[M, (Vector[Double], Int, Long), G] { case (g, op, gen) => gCons(g, Maybe.just(op), gen) })(offspringsOpsGens)
      // Replace some offsprings by clones from the original. Preferentially pick the clone with lower hit counts (the parents selected by tournament above) because
      // the evolution also keeps individual with high hitcounts as it progresses and we want to reevaluate individuals which are rare to eliminate
      // individuals that are rare by chance (in case of stochastic evaluation).
      result <- thenK(clonesReplace[M, I, G](
        cloneF = Kleisli.kleisli[M, I, G] { (i: I) =>
          for {
            // decrement the hitcount for the corresponding cell
            _ <- implicitly[HitMapper[M, C]].removeHit(cell(i))
          } yield iGenome.get(i)
        },
        cloneProbability = cloneProbability)(gs))(parents)
    } yield result
  }

  def expression[G, I](
    gValues: Lens[G, Vector[Double]],
    iCons: (G, Vector[Double]) => I)(
      fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues.get(g)))

  def elitism[M[_]: Monad: RandomGen: ({ type l[x[_]] = HitMapper[x, C] })#l, I, C](
    iGenomeValues: Lens[I, Vector[Double]],
    iGeneration: Lens[I, Long],
    cell: I => C)(
      muPerCell: Int): Objective[M, I] =
    for {
      // Update the hitmap with the newly evaluated individuals
      _ <- Kleisli.kleisli[M, Vector[I], Unit] { (is: Vector[I]) => implicitly[HitMapper[M, C]].addHits(is.map(cell)) }
      // Declone
      decloned <- applyCloneStrategy[M, I, Vector[Double]](iGenomeValues.get, keepYoungest[M, I] { iGeneration })
      // Filter out NaNs
      // TODO: flatMapPureB est assez générique pour être utilisée avec des objectifs aussi. Regrouper les fonctions génériques sur Kleisli ensemble et les renommer
      noNaNs <- thenK(flatMapPureB[M, I, I] { i: I => if (iGenomeValues.get(i).exists { (_: Double).isNaN }) Vector.empty else Vector(i) })(decloned)
      // Keep muPerCell individuals per cell randomly
      is <- thenK(byNicheO[M, I, C](cell, randomO[M, I](muPerCell)))(noNaNs)
    } yield is

  def step[M[_]: Monad: RandomGen: Generational, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Objective[M, I]): Kleisli[M, Vector[I], Vector[I]] =
    stepEA[M, I, G](
      { (_: Vector[I]) => implicitly[Generational[M]].incrementGeneration },
      breeding,
      expression,
      elitism,
      muPlusLambda[I])

  /**
   * The default PSE algorithm working with Vector[Double] genomes and patterns. (TODO: Can we abstract this to
   * make a default PSE algorithm for any types for genomes and patterns?)
   */
  object Algorithm {

    import fr.iscpif.mgo.Contexts.default._

    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], generation: Long)
    case class Individual(genome: Genome, pattern: Vector[Double])
    type HitMap = Map[Vector[Int], Int]

    val iPattern: Lens[Individual, Vector[Double]] = Lens.lensu(
      set = (i, v) => i.copy(pattern = v),
      get = _.pattern
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

    // HitMapper instance of the default Context
    implicit def mHitMap: HitMapper[EvolutionStateMonad[HitMap]#l, Vector[Int]] =
      new HitMapper[EvolutionStateMonad[HitMap]#l, Vector[Int]] {
        def get: EvolutionState[HitMap, Map[Vector[Int], Int]] =
          for {
            s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].get
          } yield s.s

        def set(newMap: Map[Vector[Int], Int]): EvolutionState[HitMap, Unit] =
          for {
            _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].modify {
              s => s.copy(s = newMap)
            }
          } yield ()

        def hitCount(cell: Vector[Int]): EvolutionState[HitMap, Int] =
          for {
            s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].get
          } yield s.s.getOrElse(cell, 0)

        def addHit(cell: Vector[Int]): EvolutionState[HitMap, Unit] =
          for {
            s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].get
            currentHitCount = s.s.getOrElse(cell, 0)
            newHitMap = s.s + ((cell, currentHitCount + 1))
            // Modify the hitMap by incrementing the corresponding value
            _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].put(s.copy(s = newHitMap))
          } yield ()

        def addHits(cells: Vector[Vector[Int]]): EvolutionState[HitMap, Unit] =
          for {
            s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].get
            newHitMap = s.s ++ (cells.map { c => (c, s.s.getOrElse(c, 0) + 1) })
            // Modify the hitMap by incrementing the corresponding values
            _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].put(s.copy(s = newHitMap))
          } yield ()

        def removeHit(cell: Vector[Int]): EvolutionState[HitMap, Unit] =
          for {
            s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].get
            currentHitCount = s.s.getOrElse(cell, 0)
            newHitMap = s.s + ((cell, currentHitCount - 1))
            // Modify the hitMap by incrementing the corresponding value
            _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[HitMap]]].put(s.copy(s = newHitMap))
          } yield ()
      }

    def cell(anchor: Vector[Double], step: Vector[Double], lowBound: Vector[Double], highBound: Vector[Double])(i: Individual): Vector[Int] =
      (i.pattern zip anchor zip step zip lowBound zip highBound).map {
        case ((((x, a), s), lb), hb) => ((max(lb, min(hb, x)) - a) / s).floor.toInt
      }

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[HitMap, Vector[Genome]] =
      PSE.initialGenomes[EvolutionStateMonad[HitMap]#l, Genome](Genome)(mu, genomeSize)
    def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double, cell: Individual => Vector[Int]): Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] =
      PSE.breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome, Vector[Int]](
        iGenome, gValues, gOperator, Genome, cell
      )(lambda, operatorExploration, cloneProbability)
    def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
      PSE.expression[Genome, Individual](gValues, Individual)(fitness)
    def elitism(muPerCell: Int, cell: Individual => Vector[Int]): Objective[EvolutionStateMonad[HitMap]#l, Individual] =
      PSE.elitism[EvolutionStateMonad[HitMap]#l, Individual, Vector[Int]](iGenomeValues, iGeneration, cell)(muPerCell)

    def step(
      mu: Int,
      lambda: Int,
      fitness: Expression[Vector[Double], Vector[Double]],
      operatorExploration: Double,
      cloneProbability: Double,
      cell: Individual => Vector[Int]): Kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Vector[Individual]] =
      PSE.step[EvolutionStateMonad[HitMap]#l, Individual, Genome](
        breeding(lambda, operatorExploration, cloneProbability, cell),
        expression(fitness),
        elitism(mu, cell)
      )

    def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = default.wrap[HitMap, A](x)
    def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = default.unwrap[HitMap, A](Map.empty[Vector[Int], Int])(x)

    def apply(
      initialPopSize: Int,
      keepPerCell: Int,
      lambda: Int,
      express: Vector[Double] => Vector[Double],
      genomeSize: Int,
      operatorExploration: Double,
      cloneProbability: Double,
      anchor: Vector[Double],
      discretisationStep: Vector[Double],
      lowBound: Vector[Double],
      highBound: Vector[Double]) =
      new Algorithm[EvolutionStateMonad[HitMap]#l, Individual, Genome, ({ type l[x] = (EvolutionData[HitMap], x) })#l] {

        implicit val m: Monad[EvolutionStateMonad[HitMap]#l] = implicitly[Monad[EvolutionStateMonad[HitMap]#l]]

        val cell: Individual => Vector[Int] = PSE.Algorithm.cell(anchor, discretisationStep, lowBound, highBound)

        def initialGenomes: EvolutionState[HitMap, Vector[Genome]] = PSE.Algorithm.initialGenomes(initialPopSize, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] = PSE.Algorithm.breeding(lambda, operatorExploration, cloneProbability, cell)
        def expression: Expression[Genome, Individual] = PSE.Algorithm.expression(express)
        def elitism: Objective[EvolutionStateMonad[HitMap]#l, Individual] = PSE.Algorithm.elitism(keepPerCell, cell)

        def step: Kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Vector[Individual]] = PSE.Algorithm.step(keepPerCell, lambda, express, operatorExploration, cloneProbability, cell)

        def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = PSE.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = PSE.Algorithm.unwrap(x)

      }

    def algoOpenMOLE(initialPopSize: Int,
      keepPerCell: Int,
      lambda: Int,
      genomeSize: Int,
      operatorExploration: Double,
      cloneProbability: Double,
      anchor: Vector[Double],
      discretisationStep: Vector[Double],
      lowBound: Vector[Double],
      highBound: Vector[Double]) =
      new AlgorithmOpenMOLE[EvolutionStateMonad[HitMap]#l, Individual, Genome, EvolutionData[HitMap]] {

        implicit val m: Monad[EvolutionStateMonad[HitMap]#l] = implicitly[Monad[EvolutionStateMonad[HitMap]#l]]

        val cell: Individual => Vector[Int] = PSE.Algorithm.cell(anchor, discretisationStep, lowBound, highBound)

        val cRandom: Lens[EvolutionData[HitMap], Random] = Lens.lensu(
          set = (e, r) => e.copy(random = r),
          get = _.random
        )

        def initialGenomes(n: Int): EvolutionState[HitMap, Vector[Genome]] = PSE.Algorithm.initialGenomes(n, genomeSize)
        def breeding(n: Int): Breeding[EvolutionStateMonad[HitMap]#l, Individual, Genome] = PSE.Algorithm.breeding(n, operatorExploration, cloneProbability, cell)
        def elitism: Objective[EvolutionStateMonad[HitMap]#l, Individual] = PSE.Algorithm.elitism(keepPerCell, cell)

        def initForIsland(i: Individual): Individual = i

        def wrap[A](x: (EvolutionData[HitMap], A)): EvolutionState[HitMap, A] = PSE.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[HitMap, A]): (EvolutionData[HitMap], A) = PSE.Algorithm.unwrap(x)

      }

  }
}
