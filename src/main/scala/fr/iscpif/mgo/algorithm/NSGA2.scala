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

import scala.language.higherKinds

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._

import scala.math._

import scalaz._
import Scalaz._

import util.Random

object NSGA2 {

  // TODO: les fonctions breeding et elitism définies dans les objets respectifs aux algos doivent être indépendantes des
  // types pour pouvoir être réutilisées ensuite dans d'autres algos. L'algorithme pure (ici NSGA2) est réellement spécifié
  // dans la fonction algorithm tout en bas. Déplacer les types ci-dessous dans cette fonction, et refactorer les fonctions
  // breeding et elitism pour qu'elle travaille sur des types abstraits I,G plutot qu'individual, et qu'elle prennent des lenses
  // pour accéder aux infos dont elles ont besoin sur les I,G, etc.

  def initialGenomes[M[_]: Monad: RandomGen, G](cons: (Vector[Double], Maybe[Int], Long) => G)(mu: Int, genomeSize: Int): M[Vector[G]] =
    for {
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      gs = values.map { (vs: Vector[Double]) => cons(vs, Maybe.empty, 0) }
    } yield gs

  // TODO: Kleisli refactoring: Un Breeding est une fonction Vector[A] => M[Vector[B]], on peut la représenter
  // en utilisant un type Kleisli[M, Vector[A], Vector[B]] et utiliser les fonctions de composition sur Kleisli. Notamment,
  // on peut enchainer deux Breedings avec b1 >> b2. Pour l'instant, on travaille à l'interieur de la monade M (cf le for ci-dessous).
  // Utiliser les fonctions de compositions définies sur Kleisli signifierait qu'on travaille au niveau du Breeding (for {_ <- b1; _ <- b2 } yield ???).
  // Ça implique un peu de refactoring: notamment, on ne pourrait plus utiliser paretoRankingMinAndCrowdingDiversity tel quel, parce qu'il
  // faut lui passer un Random, et qu'on l'extrait de M (avec split). On pourrait s'en sortir en faisant en sorte que toutes les fonctions
  // (au moins celles qu'on a besoin de composer, soit du type Kleisli[M, U,V]. Le travail dans M se ferait alors à l'intérieur des fonctions
  // individuelles (paretoRankingMinAnd... extrait elle même le Random dont elle a besoin).
  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    iFitness: Lens[I, Vector[Double]],
    iGenome: Lens[I, G],
    gValues: Lens[G, Vector[Double]],
    gOperator: Lens[G, Maybe[Int]],
    gGeneration: Lens[G, Long],
    gCons: (Vector[Double], Maybe[Int], Long) => G)(
      lambda: Int,
      operationExploration: Double): Breeding[I, M, G] =
    (individuals: Vector[I]) => {
      type V = Vector[Double]
      for {
        rg <- implicitly[RandomGen[M]].split
        generation <- implicitly[Generational[M]].getGeneration
        selected <- tournament[I, (Lazy[Int], Lazy[Double]), M](
          paretoRankingMinAndCrowdingDiversity[I] { iFitness.get }(rg),
          lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
        bred <- asB[I, (V, Maybe[Int]), M, (V, Maybe[Int]), G](
          { (i: I) => ((iGenome >=> gValues).get(i), (iGenome >=> gOperator).get(i)) },
          { case (values: V, op: Maybe[Int]) => gCons(values, op, generation) },
          dynamicallyOpB[V, M, V, (V, V), (V, V)](
            pairConsecutive[V, M],
            { case (g1, g2) => Vector(g1, g2).point[M] },
            dynamicOperators.crossoversAndMutations[M],
            operationExploration))(implicitly[Monad[M]])(selected)
        clamped = (bred: Vector[G]).map { gValues =>= { _ map { x: Double => max(0.0, min(1.0, x)) } } }
      } yield clamped
    }

  def expression[G, I](
    gValues: Lens[G, Vector[Double]],
    iCons: (G, Vector[Double]) => I)(
      fitness: Vector[Double] => Vector[Double]): Expression[G, I] =
    (g: G) => iCons(g, fitness(gValues.get(g)))

  def elitism[M[_]: Monad: RandomGen, I](
    iFitness: Lens[I, Vector[Double]],
    iGenomeValues: Lens[I, Vector[Double]],
    iGeneration: Lens[I, Long])(
      mu: Int): Objective[I, M] =
    (individuals: Vector[I]) =>
      for {
        rg <- implicitly[RandomGen[M]].split
        decloned <- applyCloneStrategy[I, Vector[Double], M](
          { iGenomeValues.get },
          clonesKeepYoungest[I, M] { iGeneration.get })(implicitly[Monad[M]])(individuals)
        noNaN = (decloned: Vector[I]).filterNot { iGenomeValues.get(_: I).exists { (_: Double).isNaN } }
        kept <- keepHighestRankedO[I, (Lazy[Int], Lazy[Double]), M](
          paretoRankingMinAndCrowdingDiversity[I] { iFitness.get }(rg),
          mu)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
      } yield kept

  /** The default NSGA2 algorithm */
  object Algorithm {

    import fr.iscpif.mgo.Contexts.default._

    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], generation: Long)
    case class Individual(genome: Genome, fitness: Vector[Double])

    val iFitness: Lens[Individual, Vector[Double]] = Lens.lensu(
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

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] = NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l, Genome](Genome)(mu, genomeSize)
    def breeding(lambda: Int, operationExploration: Double): Breeding[Individual, EvolutionStateMonad[Unit]#l, Genome] = NSGA2.breeding[EvolutionStateMonad[Unit]#l, Individual, Genome](
      iFitness,
      iGenome,
      gValues,
      gOperator,
      gGeneration,
      Genome
    )(lambda, operationExploration)
    def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] = NSGA2.expression[Genome, Individual](
      gValues,
      Individual
    )(fitness)
    def elitism(mu: Int): Objective[Individual, EvolutionStateMonad[Unit]#l] = NSGA2.elitism[EvolutionStateMonad[Unit]#l, Individual](
      iFitness,
      iGenomeValues,
      iGeneration
    )(mu)

    def step(
      mu: Int,
      lambda: Int,
      fitness: Expression[Vector[Double], Vector[Double]],
      operationExploration: Double): Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
      stepEA[Individual, EvolutionStateMonad[Unit]#l, Genome](
        { (_: Vector[Individual]) => implicitly[Generational[EvolutionStateMonad[Unit]#l]].incrementGeneration },
        breeding(lambda, operationExploration),
        expression(fitness),
        elitism(mu),
        muPlusLambda[Individual])

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(fitness: Vector[Double] => Vector[Double], mu: Int, lambda: Int, genomeSize: Int, operationExploration: Double) =
      new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = NSGA2.Algorithm.initialGenomes(mu, genomeSize)
        def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Genome] = NSGA2.Algorithm.breeding(lambda, operationExploration)
        def expression: Expression[Genome, Individual] = NSGA2.Algorithm.expression(fitness)
        def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NSGA2.Algorithm.elitism(mu)

        def step: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] = NSGA2.Algorithm.step(mu, lambda, fitness, operationExploration)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NSGA2.Algorithm.unwrap(x)

      }
  }
}
