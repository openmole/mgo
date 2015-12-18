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
import fr.iscpif.mgo.Expressions._
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

  def initialGenomes[M[_]: Monad: RandomGen, G](gCons: (Vector[Double], Maybe[Int], Long) => G)(mu: Int, genomeSize: Int): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      genomes = values.map { (vs: Vector[Double]) => gCons(vs, Maybe.empty, 0) }
    } yield genomes

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    iFitness: Lens[I, Double],
    iGenome: Lens[I, G],
    gValues: Lens[G, Vector[Double]],
    gOperator: Lens[G, Maybe[Int]],
    gCons: (Vector[Double], Maybe[Int], Long) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => {
      type V = Vector[Double]
      for {
        rg <- implicitly[RandomGen[M]].split
        generation <- implicitly[Generational[M]].getGeneration
        selected <- tournament[M, I, Lazy[Int]](
          ranking = profileRanking[I](niche, iFitness.get(_: I)),
          size = lambda,
          rounds = size => math.round(math.log10(size).toInt))(implicitly[Monad[M]], implicitly[RandomGen[M]], implicitly[Order[Lazy[Int]]])(individuals)
        bred <- asB[M, I, (V, Maybe[Int]), (V, Maybe[Int]), G](
          { (i: I) => ((iGenome >=> gValues).get(i), (iGenome >=> gOperator).get(i)) },
          { (valuesop: (V, Maybe[Int])) => gCons(valuesop._1, valuesop._2, generation) },
          dynamicallyOpB[M, V, V, (V, V), (V, V)](
            pairConsecutive[M, V],
            Kleisli.kleisli[M,(V,V),Vector[V]]{ case (g1, g2) => Vector(g1, g2).point[M] } ,
            dynamicOperators.crossoversAndMutations[M],
            operatorExploration))(implicitly[Monad[M]])(selected)
        clamped = (bred: Vector[G]).map { gValues =>= { _ map { x: Double => max(0.0, min(1.0, x)) } } }
      } yield clamped
    })

  def expression[G, I](
    gValues: Lens[G, Vector[Double]],
    iCons: (G, Double) => I)(
      fitness: Vector[Double] => Double): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues.get(g)))

  def elitism[M[_]: Monad: RandomGen, I](
    iFitness: Lens[I, Double],
    iGenomeValues: Lens[I, Vector[Double]],
    iGeneration: Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int]): Objective[M, I] =
    byNicheO[I, Int, M](
      niche = niche,
      objective = Objective((individuals: Vector[I]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[M, I, Vector[Double]](
            { (i: I) => iGenomeValues.get(i) },
            keepYoungest[M, I] { iGeneration })(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[I]).filterNot { iGenomeValues.get(_).exists { (_: Double).isNaN } }
          kept <- minimiseO[M, I, Double](
            { i: I => iFitness.get(i) },
            muByNiche)(implicitly[Monad[M]], implicitly[Order[Double]])(noNaN)
        } yield kept)
    )

  def step[M[_]: Monad: RandomGen: Generational, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Objective[M, I]): Vector[I] => M[Vector[I]] =
    stepEA[M, I, G](
      { (_: Vector[I]) => implicitly[Generational[M]].incrementGeneration },
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
      operatorExploration: Double): Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
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

        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = Profile.Algorithm.initialGenomes(muByNiche, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, Genome] = Profile.Algorithm.breeding(lambda, niche, operatorExploration)
        def expression: Expression[Genome, Individual] = Profile.Algorithm.expression(fitness)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = Profile.Algorithm.elitism(muByNiche, niche)

        def step: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] = Profile.Algorithm.step(muByNiche, lambda, fitness, niche, operatorExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = Profile.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = Profile.Algorithm.unwrap(x)

      }
  }
}
