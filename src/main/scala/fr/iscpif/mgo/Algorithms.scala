/*
 * Copyright (C) 04/12/2015 Guillaume Chérel
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
package fr.iscpif.mgo

import fr.iscpif.mgo.tools.Lazy

import scala.language.higherKinds
import scalaz._
import Scalaz._

import Breedings._
import Expressions._
import Objectives._
import ranking._
import Contexts._
import Contexts.default._

import scala.math.{ min, max }

object Algorithms {

  object GenomeVectorDouble {
    def randomGenomes[M[_]: Monad: RandomGen](n: Int, genomeLength: Int): M[Vector[Vector[Double]]] =
      for {
        rg <- implicitly[RandomGen[M]].split
        values = Vector.fill(n)(Vector.fill(genomeLength)(rg.nextDouble))
      } yield values
  }

  object NSGA2 {

    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], generation: Long)
    case class Individual(genome: Genome, fitness: Vector[Double])

    def initialGenomes[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Genome]] =
      for {
        values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
        genomes = values.map { (vs: Vector[Double]) => Genome(vs, Maybe.empty, 0) }
      } yield genomes

    def crossovers[M[_]: Monad: RandomGen]: Vector[Crossover[(V, V), M, (V, V)]] =
      Vector(
        replicatePairC(blxC(0.1)),
        replicatePairC(blxC(0.5)),
        replicatePairC(blxC(2.0)),
        sbxC(0.1),
        sbxC(0.5),
        sbxC(2.0)
      )

    def mutations[M[_]: Monad: RandomGen]: Vector[Mutation[V, M, V]] =
      Vector(
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.001),
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.01),
        bgaM(mutationRate = 2.0 / _, mutationRange = 0.1),
        bgaM(mutationRate = _ => 0.5, mutationRange = 0.5)
      )

    def crossoversAndMutations[M[_]: Monad: RandomGen]: Vector[((V, V)) => M[(V, V)]] =
      for {
        c <- crossovers[M]
        m <- mutations[M]
      } yield {
        (mates: (V, V)) =>
          for {
            crossed <- c(mates)
            m1 <- m(crossed._1)
            m2 <- m(crossed._2)
          } yield (m1, m2)
      }

    def breeding[M[_]: Monad: RandomGen: Generational](
      lambda: Int,
      operationExploration: Double = 0.1): Breeding[Individual, M, Genome] =
      (individuals: Vector[Individual]) => {

        for {
          rg <- implicitly[RandomGen[M]].split
          generation <- implicitly[Generational[M]].getGeneration
          selected <- tournament[Individual, (Lazy[Int], Lazy[Double]), M](
            paretoRankingAndCrowdingDiversity[Individual] { (i: Individual) => i.fitness }(rg),
            lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
          bred <- liftB[Individual, (V, Maybe[Int]), M, (V, Maybe[Int]), Genome](
            { (i: Individual) => (i.genome.values, i.genome.operator) },
            { case (values: V, op: Maybe[Int]) => Genome(values, op, generation) },
            dynamicallyOpB[V, M, V, (V, V), (V, V)](
              pairConsecutive[V, M],
              { case (g1, g2) => Vector(g1, g2).point[M] },
              crossoversAndMutations[M],
              operationExploration))(implicitly[Monad[M]])(selected)
          clamped = (bred: Vector[Genome]).map { (g: Genome) => g.copy(values = g.values.map { x: Double => max(0.0, min(1.0, x)) }) }
        } yield clamped
      }

    def elitism[M[_]: Monad: RandomGen](mu: Int): Objective[Individual, M] =
      (individuals: Vector[Individual]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[Individual, Genome, M](
            { (i: Individual) => i.genome },
            clonesKeepYoungest[Individual, M] { (i: Individual) => i.genome.generation })(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.values.exists { (_: Double).isNaN } }
          kept <- keepBestByO[Individual, (Lazy[Int], Lazy[Double]), M](
            paretoRankingAndCrowdingDiversity[Individual] { (i: Individual) => i.fitness }(rg),
            mu)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
        } yield kept

    def step[M[_]: Monad: RandomGen: Generational](
      fitness: Genome => Vector[Double],
      mu: Int,
      lambda: Int): Vector[Individual] => M[Vector[Individual]] =
      stepEA[Individual, M, Genome](
        { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
        breeding[M](lambda),
        { (g: Genome) => Individual(g, fitness(g)) },
        elitism[M](mu),
        muPlusLambda[Individual])

    def algorithm(mu: Int, lambda: Int, genomeSize: Int) =
      new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Genome, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[Genome]] = NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l](mu, genomeSize)
        def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Genome] = NSGA2.breeding[EvolutionStateMonad[Unit]#l](lambda)
        def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NSGA2.elitism[EvolutionStateMonad[Unit]#l](mu)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)

      }
  }

  object NoisyNSGA2 {

    type V = Vector[Double]
    case class Genome(values: V, operator: Maybe[Int], age: Long)
    case class Individual(genome: Genome, fitnessHistory: Vector[Vector[Double]])

    implicit val individualAge: Age[Individual] = new Age[Individual] {
      def getAge(i: Individual): Long = i.genome.age
      def setAge(i: Individual, a: Long): Individual = i.copy(genome = i.genome.copy(age = a))
    }

    implicit val individualHistory: PhenotypeHistory[Individual, Vector[Double]] = new PhenotypeHistory[Individual, Vector[Double]] {
      def getPhenotype(i: Individual): Vector[Double] = i.fitnessHistory.last
      def append(i: Individual, p: Vector[Double]): Individual = i.copy(fitnessHistory = i.fitnessHistory :+ p)
      def getHistory(i: Individual): Vector[Vector[Double]] = i.fitnessHistory
      def setHistory(i: Individual, h: Vector[Vector[Double]]): Individual = i.copy(fitnessHistory = h)
    }

    def fitnessWithReplications(i: Individual): Vector[Double] = individualHistory.getPhenotype(i) ++ Seq(1.0 / individualHistory.getHistory(i).size)

    def initialPopulation[M[_]: Monad: RandomGen](mu: Int, genomeSize: Int): M[Vector[Individual]] =
      for {
        values <- GenomeVectorDouble.randomGenomes[M](mu, genomeSize)
        indivs = values.map { (vs: Vector[Double]) => Individual(genome = Genome(values = vs, operator = Maybe.empty, age = 0), fitnessHistory = Vector.empty) }
      } yield indivs

    def crossovers[M[_]: Monad: RandomGen]: Vector[Crossover[(V, V), M, (V, V)]] =
      Vector(
        replicatePairC(blxC(0.1)),
        replicatePairC(blxC(0.5)),
        replicatePairC(blxC(2.0)),
        sbxC(0.1),
        sbxC(0.5),
        sbxC(2.0)
      )

    def mutations[M[_]: Monad: RandomGen]: Vector[Mutation[V, M, V]] =
      Vector(
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.001),
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.01),
        bgaM(mutationRate = 2.0 / _, mutationRange = 0.1),
        bgaM(mutationRate = _ => 0.5, mutationRange = 0.5)
      )

    def crossoversAndMutations[M[_]: Monad: RandomGen]: Vector[((V, V)) => M[(V, V)]] =
      for {
        c <- crossovers[M]
        m <- mutations[M]
      } yield {
        (mates: (V, V)) =>
          for {
            crossed <- c(mates)
            m1 <- m(crossed._1)
            m2 <- m(crossed._2)
          } yield (m1, m2)
      }

    def breeding[M[_]: Monad: RandomGen: Generational](
      lambda: Int,
      operationExploration: Double = 0.1,
      cloneProbability: Double = 0.1): Breeding[Individual, M, Individual] =
      (individuals: Vector[Individual]) => {

        for {
          rg <- implicitly[RandomGen[M]].split
          selected <- tournament[Individual, (Lazy[Int], Lazy[Double]), M](
            paretoRankingAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
            lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]], implicitly[RandomGen[M]])(individuals)
          // Ce qui suit est compliqué parce qu'il y a plusieurs étapes de fonctions imbriquées: On utilise liftB pour
          // transformer un Individual en (Individual, Operateur) pour correspondre à la signature de dynamicallyOpB. Puis, on
          // associe les individus par paires parceque les operateurs qu'on utilise ensuite (crossover + mutation) prennent des
          // paires et renvoiens des paires. Ensuite, on map sur les operateurs crossoversAndMutations pour les transformer en operateurs
          // qui prennent et renvoient des paires d'individus plutôt que de Vecteur[Double]. L'avantage de cette approche est que les fonctions individuelles
          // sont minimalistes, le désavantage est qu'il y a beaucoup de travail pour les combiner ensemble. On pourrait peut-être gagner en simplicité sur
          // cette transformation de signatures avec une autre approche: par exemple, si toutes les fonctions de breeding prennent des fonctions
          // qui leur permettent d'extraire les info dont elles ont besoin à partir des individus (ce que fait actuellement liftB), ou des lenses.
          bred <- liftB[Individual, (Individual, Maybe[Int]), M, (Individual, Maybe[Int]), Individual](
            { (i: Individual) => (i, i.genome.operator) },
            { case (i: Individual, op: Maybe[Int]) => i.copy(genome = i.genome.copy(operator = op, age = 0)) },
            dynamicallyOpB[Individual, M, Individual, (Individual, Individual), (Individual, Individual)](
              pairConsecutive[Individual, M],
              { case (i1, i2) => Vector(i1, i2).point[M] },
              crossoversAndMutations[M].map {
                op =>
                  opOrClone[(Individual, Individual), M, (Individual, Individual)](
                    getGenome = {
                      case (i1: Individual, i2: Individual) =>
                        def age(i: Individual): Individual = i.copy(genome = i.genome.copy(age = i.genome.age + 1))
                        (age(i1), age(i2))
                    },
                    op = {
                      case (i1: Individual, i2: Individual) =>
                        for {
                          newg1g2 <- op(i1.genome.values, i2.genome.values)
                          (newg1, newg2) = newg1g2
                          newi1 = i1.copy(genome = i1.genome.copy(values = newg1, age = 0), fitnessHistory = Vector.empty)
                          newi2 = i2.copy(genome = i2.genome.copy(values = newg2, age = 0), fitnessHistory = Vector.empty)
                        } yield (newi1, newi2)
                    },
                    cloneProbability = cloneProbability)
              },
              operationExploration))(implicitly[Monad[M]])(selected)
          clamped = (bred: Vector[Individual]).map { (i: Individual) => i.copy(genome = i.genome.copy(values = i.genome.values.map { x: Double => max(0.0, min(1.0, x)) })) }
        } yield clamped
      }

    def elitism[M[_]: Monad: RandomGen](mu: Int, historySize: Int): Objective[Individual, M] =
      (individuals: Vector[Individual]) =>
        for {
          rg <- implicitly[RandomGen[M]].split
          decloned <- applyCloneStrategy[Individual, Genome, M](
            { (i: Individual) => i.genome },
            clonesMergeHistories[Individual, V, M](historySize))(implicitly[Monad[M]])(individuals)
          noNaN = (decloned: Vector[Individual]).filterNot { (_: Individual).genome.values.exists { (_: Double).isNaN } }
          kept <- keepBestByO[Individual, (Lazy[Int], Lazy[Double]), M](
            paretoRankingAndCrowdingDiversity[Individual] { fitnessWithReplications }(rg),
            mu)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(noNaN)
        } yield kept

    def step[M[_]: Monad: RandomGen: Generational](
      fitness: Individual => Vector[Double],
      mu: Int,
      lambda: Int,
      historySize: Int): Vector[Individual] => M[Vector[Individual]] =
      stepEA[Individual, M, Individual](
        { (_: Vector[Individual]) => implicitly[Generational[M]].incrementGeneration },
        breeding[M](lambda),
        historyE(fitness),
        elitism[M](mu, historySize),
        muPlusLambda[Individual])

    def algorithm(mu: Int, lambda: Int, genomeSize: Int, historySize: Int) =
      new Algorithm[Individual, EvolutionStateMonad[Unit]#l, Individual, ({ type l[x] = (EvolutionData[Unit], x) })#l] {
        implicit val m: Monad[EvolutionStateMonad[Unit]#l] = implicitly[Monad[EvolutionStateMonad[Unit]#l]]

        def initialGenomes: EvolutionState[Unit, Vector[Individual]] = NoisyNSGA2.initialPopulation[EvolutionStateMonad[Unit]#l](mu, genomeSize)
        def breeding: Breeding[Individual, EvolutionStateMonad[Unit]#l, Individual] = NoisyNSGA2.breeding[EvolutionStateMonad[Unit]#l](lambda)
        def elitism: Objective[Individual, EvolutionStateMonad[Unit]#l] = NoisyNSGA2.elitism[EvolutionStateMonad[Unit]#l](mu, historySize)

        def wrap[A](x: (EvolutionData[Unit], A)): EvolutionState[Unit, A] = default.wrap[Unit, A](x)
        def unwrap[A](x: EvolutionState[Unit, A]): (EvolutionData[Unit], A) = default.unwrap[Unit, A](())(x)
      }
  }
}
