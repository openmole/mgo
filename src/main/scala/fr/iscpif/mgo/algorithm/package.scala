/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

import fr.iscpif.mgo.fitness.Fitness
import fr.iscpif.mgo.tools.Lazy

import scala.util.Random
import scalaz._
import genome._
import ranking._
import diversity._
import clone._
import breeding._
import elitism._
import niche._
import tools._
import breeding._
import mutation._
import crossover._

package object algorithm {

  object ga {
    case class GAGenome(values: Array[Double] @@ genome.Value, fromOperation: Int = -1)

    implicit val genomeValues =
      monocle.Lens[GAGenome, Seq[Double] @@ genome.Value](g => unwrap(g.values).toSeq)(v => _.copy(values = wrap(v.toArray)))

    implicit def equalsG = Equal.equal[GAGenome]((g1, g2) => g1.genomeValue == g2.genomeValue)

    def intToOption = monocle.Lens[Int, Option[Int]](b => if (b < 0) None else Some(b))(o => _ => o.getOrElse(-1))

    def fromOperation = monocle.macros.Lenser[GAGenome](_.fromOperation) composeLens intToOption

    def randomGenome(size: Int) = State { rng: Random =>
      def genome = GAGenome(wrap(Seq.fill(size)(rng.nextDouble).toArray))
      (rng, genome)
    }

    def newGenomes[P, S](selection: State[AlgorithmState[S], Individual[GAGenome, P]], pop: Population[Individual[GAGenome, P]], operationExploration: Double): State[AlgorithmState[S], Vector[GAGenome]] =
      for {
        op <- random[S] lifts dynamicOperator(pop, fromOperation, operationExploration, operations[S].zipWithIndex)
        ((c, m), i) = op
        s1 <- selection
        s2 <- selection
        res <- breed(c, m)(s1, s2)
      } yield res.map(fromOperation.set(Some(i))).map(g => clamp(g)).toVector

    def operations[S]: Vector[(Crossover[GAGenome, S], Mutation[GAGenome, S])] =
      for {
        c <- crossover[S]
        m <- mutations[S]
      } yield (c, m)

    def mutations[S] = Vector[Mutation[GAGenome, S]](
      bga[GAGenome, S](mutationRate = 1.0 / _, mutationRange = 0.001),
      bga[GAGenome, S](mutationRate = 1.0 / _, mutationRange = 0.01),
      bga[GAGenome, S](mutationRate = 2.0 / _, mutationRange = 0.1),
      bga[GAGenome, S](mutationRate = _ => 0.5, mutationRange = 0.5)
    )

    def crossover[S]: Vector[Crossover[GAGenome, S]] = Vector(
      blx[GAGenome, S](0.1),
      blx[GAGenome, S](0.5),
      blx[GAGenome, S](2.0),
      sbx[GAGenome, S](0.1),
      sbx[GAGenome, S](0.5),
      sbx[GAGenome, S](2.0)
    )

    def NSGA2[P](
      mu: Int,
      fitness: Fitness[GAGenome, P, Seq[Double]],
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, P, Unit] {
      def initialState = Unit

      def ranking = paretoRanking(fitness)

      def diversity = crowdingDistance(fitness)

      override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
        val challenge =
          for {
            c1 <- onRank(ranking)(pop)
            c2 <- onDiversity(diversity)(pop)
          } yield c1 and c2

        (random[Unit] lifts challenge).flatMap { challenged =>
          def fight = random[Unit] lifts tournament(challenged, pop)
          newGenomes[P, Unit](fight, pop, operationExploration).generateFlat(lambda)
        }
      }

      override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] =
        for {
          _ <- State.init[AlgorithmState[Unit]]
          p1 = merge(population, offspring)
          p2 = applyCloneStrategy(p1, youngest)
          p3 = removeNaN(p2, fitness)
          p4 <- random[Unit] lifts keepNonDominated(mu, ranking, diversity)(p3)
        } yield p4
    }

    def noisyNSGA2[P](
      mu: Int,
      fitness: Fitness[GAGenome, History[P], Seq[Double]],
      history: Int,
      cloneRate: Double = 0.2,
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, History[P], Unit] {
      def initialState = Unit

      def fitnessWithReplications(i: Ind) = fitness(i) ++ Seq(1.0 / i.phenotype.size)

      def ranking: Ranking[GAGenome, History[P]] = paretoRanking(fitnessWithReplications)

      def diversity: Diversity[GAGenome, History[P]] = crowdingDistance(fitnessWithReplications)

      def cloneStrategy = queue[P](size = history)

      override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
        val challenge =
          for {
            c1 <- onRank(ranking)(pop)
            c2 <- onDiversity(diversity)(pop)
          } yield c1 and c2

        (random[Unit] lifts challenge).flatMap { challenged =>
          def fight = random[Unit] lifts tournament(challenged, pop)

          interleaveClones[GAGenome, History[P], Unit](
            newGenomes[History[P], Unit](fight, pop, operationExploration),
            fight.map(_.genome),
            lambda,
            cloneRate)
        }
      }

      override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] =
        for {
          _ <- State.init[AlgorithmState[Unit]]
          p1 = merge(population, offspring)
          p2 = applyCloneStrategy(p1, cloneStrategy)
          p3 = removeNaN(p2, fitness)
          p4 <- random[Unit] lifts keepNonDominated(mu, ranking, diversity)(p3)
        } yield p4
    }

    def profile[P](
      fitness: Fitness[GAGenome, P, Double],
      niche: Niche[GAGenome, P, Int],
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, P, Unit] {
      def initialState = Unit

      implicit val nicheEqual = Equal.equal[Int](_ == _)

      override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
        val challenge = onRank(profileRanking(niche, fitness))(pop)

        (random[Unit] lifts challenge).flatMap { challenged =>
          def fight = random[Unit] lifts tournament(challenged, pop, size => math.round(math.log10(size).toInt))
          newGenomes[P, Unit](fight, pop, operationExploration).generateFlat(lambda)
        }
      }

      override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] = {
        val p1 = merge(population, offspring)
        val p2 = applyCloneStrategy(p1, youngest)
        val p3 = removeNaN(p2, fitness)

        def keep(p: Pop) = State.gets { s: AlgorithmState[Unit] => keepBest(1, fitness)(p) }

        for { p4 <- nicheElitism(keep, p3, niche) } yield p4
      }
    }

    def noisyProfile[P](
      fitness: Fitness[GAGenome, History[P], Double],
      niche: Niche[GAGenome, History[P], Int],
      nicheSize: Int,
      history: Int,
      cloneRate: Double = 0.2,
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, History[P], Unit] {
      def initialState = Unit

      implicit val nicheEqual = Equal.equal[Int](_ == _)

      def fitnessWithReplications(i: Ind) = Seq(fitness(i), 1.0 / i.phenotype.size)

      def ranking: Ranking[GAGenome, History[P]] = paretoRanking(fitnessWithReplications)
      def diversity: Diversity[GAGenome, History[P]] = crowdingDistance(fitnessWithReplications)

      def cloneStrategy = queue[P](size = history)

      override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
        val challenge =
          for {
            c1 <- onRank(ranking)(pop)
            c2 <- onDiversity(diversity)(pop)
          } yield c1 and c2

        (random[Unit] lifts challenge).flatMap { challenged =>
          def fight = random[Unit] lifts tournament(challenged, pop, size => math.round(math.log10(size).toInt))

          interleaveClones[GAGenome, History[P], Unit](
            newGenomes[History[P], Unit](fight, pop, operationExploration),
            random[Unit] lifts generateClone(pop),
            lambda,
            cloneRate)
        }
      }

      override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] = {
        val p1 = merge(population, offspring)
        val p2 = applyCloneStrategy(p1, cloneStrategy)
        val p3 = removeNaN(p2, fitness)

        def keep(p: Pop) = random[Unit] lifts keepNonDominated(nicheSize, ranking, diversity)(p)

        for { p4 <- nicheElitism(keep, p3, niche) } yield p4
      }
    }

    def PSE[P, Point](
      niche: Niche[GAGenome, P, Point],
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, P, collection.Map[Point, Int]] {
      implicit val pointEqual = Equal.equal[Point](_ == _)

      def initialState = Map()

      override def breeding(pop: Population[Individual[GAGenome, P]], lambda: Int) =
        onHitCount(identityLens[collection.Map[Point, Int]], niche)(pop) flatMap { challenged =>
          def fight = random[collection.Map[Point, Int]] lifts tournament(challenged, pop, size => math.round(math.log10(size).toInt))
          newGenomes(fight, pop, operationExploration).generateFlat(lambda)
        }

      override def elitism(population: Population[Individual[GAGenome, P]], offspring: Population[Individual[GAGenome, P]]) = {
        val p1 = merge(population, offspring)
        val p2 = applyCloneStrategy(p1, youngest)

        def keep(p: Pop) = random[collection.Map[Point, Int]] lifts keepRandom(1)(p)

        for { p3 <- nicheElitism(keep, p2, niche) } yield p3
      }
    }

    def noisyPSE[P, Point](
      niche: Niche[GAGenome, P, Point],
      history: Int,
      cloneRate: Double = 0.2,
      operationExploration: Double = 0.1) = new Algorithm[GAGenome, P, collection.Map[Point, Int]] {
      implicit val pointEqual = Equal.equal[Point](_ == _)

      def initialState = Map()
      def cloneStrategy = queue[P](size = history)

      override def breeding(pop: Population[Individual[GAGenome, P]], lambda: Int) =
        onHitCount(identityLens[collection.Map[Point, Int]], niche)(pop) flatMap { challenged =>
          def fight = random[collection.Map[Point, Int]] lifts tournament(challenged, pop, size => math.round(math.log10(size).toInt))

          interleaveClones[GAGenome, History[P], collection.Map[Point, Int]](
            newGenomes(fight, pop, operationExploration),
            random[collection.Map[Point, Int]] lifts generateClone(pop),
            lambda,
            cloneRate)
        }

      override def elitism(population: Population[Individual[GAGenome, P]], offspring: Population[Individual[GAGenome, P]]) = {
        val p1 = merge(population, offspring)
        val p2 = applyCloneStrategy(p1, youngest)

        def keep(p: Pop) = random[collection.Map[Point, Int]] lifts keepRandom(1)(p)

        for { p3 <- nicheElitism(keep, p2, niche) } yield p3
      }
    }
  }
}
