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

import scala.util.Random

import scalaz._
import Scalaz._

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Expressions._
import fr.iscpif.mgo.Objectives._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.Contexts.default._

import scala.math._

object NoisyNSGA2 {

  def fitnessWithReplications[I](
    iFitness: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]])(i: I): Vector[Double] = iFitness.get(i) ++ Vector(1.0 / iHistory.get(i).size.toDouble)

  def initialGenomes[M[_]: Monad: RandomGen, I](
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Vector[Double]]) => I)(mu: Int, genomeSize: Int): M[Vector[(Random, I)]] =
    for {
      rgs <- implicitly[RandomGen[M]].split.replicateM(mu)
      values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
      indivs = rgs.toVector zip values.map { vs: Vector[Double] => iCons(vs, Maybe.empty, 1, Vector.empty) }
    } yield indivs

  def breeding[M[_]: Monad: RandomGen: Generational, I](
    iFitness: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]],
    iValues: Lens[I, Vector[Double]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long],
    iCons: (Vector[Double], Maybe[Int], Long, Vector[Vector[Double]]) => I)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, (Random, I)] =
    withRandomGenB[M, I, I](
      Breeding(
        (individuals: Vector[I]) => {

          //TODO: Vérifier le sens de paretoRanking
          for {
            rg <- implicitly[RandomGen[M]].split
            selected <- tournament[M, I, (Lazy[Int], Lazy[Double])](
              paretoRankingMinAndCrowdingDiversity[I] { fitnessWithReplications(iFitness, iHistory) }(rg),
              lambda)(implicitly[Monad[M]], implicitly[RandomGen[M]], implicitly[Order[(Lazy[Int], Lazy[Double])]])(individuals)
            // Ce qui suit est compliqué parce qu'il y a plusieurs étapes de fonctions imbriquées: On utilise liftB pour
            // transformer un I en (I, Operateur) pour correspondre à la signature de dynamicallyOpB. Puis, on
            // associe les individus par paires parceque les operateurs qu'on utilise ensuite (crossover + mutation) prennent des
            // paires et renvoiens des paires. Ensuite, on map sur les operateurs crossoversAndMutations pour les transformer en operateurs
            // qui prennent et renvoient des paires d'individus plutôt que de Vecteur[Double]. L'avantage de cette approche est que les fonctions individuelles
            // sont minimalistes, et aussi que la signature des fonction nous donne beaucoup d'info sur ce que fait la fonction, le
            // désavantage est qu'il y a beaucoup de travail pour les combiner ensemble. On pourrait peut-être gagner en simplicité sur
            // cette transformation de signatures avec une autre approche: par exemple, si toutes les fonctions de breeding prennent des fonctions
            // qui leur permettent d'extraire les info dont elles ont besoin à partir des individus (ce que fait actuellement liftB), ou des lenses.
            bred <- asB[ M,I, (I, Maybe[Int]), (I, Maybe[Int]), I](
              { i => (i, iOperator.get(i)) },
              { case (i, op) => iOperator.set(i, op) },
              dynamicallyOpB[M, I, I, (I, I), (I, I)](
                pairConsecutive[M, I],
                Kleisli.kleisli[M,(I,I),Vector[I]]{ case (i1, i2) => Vector(i1, i2).point[M] },
                dynamicOperators.crossoversAndMutations[M].map {
                  op =>
                    opOrClone[M, (I, I), (I, I)](
                      // cloning copies the whole individual as is and increments its age
                      clone = {
                        case (i1, i2) =>
                          def age(i: I): I = iAge.mod(_ + 1, i)
                          (age(i1), age(i2))
                      },
                      op = {
                        case (i1, i2) =>
                          for {
                            newg1g2 <- op(iValues.get(i1), iValues.get(i2))
                            (newg1, newg2) = newg1g2
                            newi1 = iHistory.set(iValues.set(i1, newg1), Vector.empty)
                            newi2 = iHistory.set(iValues.set(i2, newg2), Vector.empty)
                          } yield (newi1, newi2)
                      },
                      cloneProbability = cloneProbability)
                },
                operatorExploration))(implicitly[Monad[M]])(selected)
            clamped = (bred: Vector[I]).map { iValues =>= { _ map { x: Double => max(0.0, min(1.0, x)) } } }
          } yield clamped
        }
      )
    )

  def expression[I](
    iValues: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]])(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, I), I] =
    { case (rg, i) => iHistory.mod(_ :+ fitness(rg, iValues.get(i)), i) }

  def elitism[M[_]: Monad: RandomGen, I](
    iValues: Lens[I, Vector[Double]],
    iFitness: Lens[I, Vector[Double]],
    iHistory: Lens[I, Vector[Vector[Double]]],
    iOperator: Lens[I, Maybe[Int]],
    iAge: Lens[I, Long])(mu: Int, historySize: Int): Objective[M, I] =
    Objective((individuals: Vector[I]) =>
      for {
        rg <- implicitly[RandomGen[M]].split
        decloned <- applyCloneStrategy[M,I, Vector[Double]](
          { (i: I) => iValues.get(i) },
          mergeHistories[M, I, Vector[Double]](iAge, iHistory)(historySize))(implicitly[Monad[M]])(individuals)
        noNaN = (decloned: Vector[I]).filterNot { iValues.get(_).exists { (_: Double).isNaN } }
        kept <- keepHighestRankedO[M, I, (Lazy[Int], Lazy[Double])](
          paretoRankingMinAndCrowdingDiversity[I] { fitnessWithReplications(iFitness, iHistory) }(rg),
          mu)(implicitly[Monad[M]], implicitly[Order[(Lazy[Int], Lazy[Double])]])(noNaN)
      } yield kept)

  def step[M[_]: Monad: Generational: RandomGen, I, G](
    breeding: Breeding[M, I, (Random, G)],
    expression: Expression[(Random, G), I],
    elitism: Objective[M, I]): Vector[I] => M[Vector[I]] =
    stepEA[M, I, (Random, G)](
      { (_: Vector[I]) => implicitly[Generational[M]].incrementGeneration },
      breeding,
      expression,
      elitism,
      muPlusLambda[I])

  object Algorithm {
    case class Individual(genome: Vector[Double], operator: Maybe[Int], age: Long, fitnessHistory: Vector[Vector[Double]])

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
    val iHistory: Lens[Individual, Vector[Vector[Double]]] = Lens.lensu(
      set = (i, h) => i.copy(fitnessHistory = h),
      get = _.fitnessHistory
    )
    val iFitness: Lens[Individual, Vector[Double]] = Lens.lensu(
      set = (i, f) => i.copy(fitnessHistory = i.fitnessHistory.dropRight(1) :+ f),
      get = _.fitnessHistory.last
    )

    def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[Unit, Vector[(Random, Individual)]] =
      NoisyNSGA2.initialGenomes[EvolutionStateMonad[Unit]#l, Individual](Individual)(mu, genomeSize)
    def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double): Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] =
      NoisyNSGA2.breeding[EvolutionStateMonad[Unit]#l, Individual](
        iFitness, iHistory, iValues, iOperator, iAge, Individual
      )(lambda, operatorExploration, cloneProbability)
    def expression(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, Individual), Individual] =
      NoisyNSGA2.expression[Individual](iValues, iHistory)(fitness)
    def elitism(mu: Int, historySize: Int): Objective[EvolutionStateMonad[Unit]#l, Individual] =
      NoisyNSGA2.elitism[EvolutionStateMonad[Unit]#l, Individual](iValues, iFitness, iHistory, iOperator, iAge)(mu, historySize)

    def step(
      mu: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Vector[Double],
      operatorExploration: Double,
      historySize: Int,
      cloneProbability: Double): Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
      NoisyNSGA2.step[EvolutionStateMonad[Unit]#l, Individual, Individual](
        breeding(lambda, operatorExploration, cloneProbability),
        expression(fitness),
        elitism(mu, historySize))

    def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
    def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

    def apply(
      mu: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Vector[Double],
      operatorExploration: Double,
      genomeSize: Int,
      historySize: Int,
      cloneProbability: Double) =
      new Algorithm[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual), ({ type l[x] = (EvolutionData[Unit], x) })#l] {

        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[(Random, Individual)]] = NoisyNSGA2.Algorithm.initialGenomes(mu, genomeSize)
        def breeding: Breeding[EvolutionStateMonad[Unit]#l, Individual, (Random, Individual)] = NoisyNSGA2.Algorithm.breeding(lambda, operatorExploration, cloneProbability)
        def expression: Expression[(Random, Individual), Individual] = NoisyNSGA2.Algorithm.expression(fitness)
        def elitism: Objective[EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.Algorithm.elitism(mu, historySize)

        def step: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] = NoisyNSGA2.Algorithm.step(mu, lambda, fitness, operatorExploration, historySize, cloneProbability)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = NoisyNSGA2.Algorithm.wrap(x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = NoisyNSGA2.Algorithm.unwrap(x)
      }
  }

}

