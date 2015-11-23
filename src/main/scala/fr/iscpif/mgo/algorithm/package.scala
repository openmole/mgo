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

import fr.iscpif.mgo.algorithm.ga._
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

package object algorithm {

  def nsga2[P](
    mu: Int,
    fitness: Fitness[GAGenome, P, Seq[Double]])(
      operationExploration: Double = 0.1,
      ranking: Ranking[GAGenome, P] = paretoRanking(fitness = fitness),
      diversity: Diversity[GAGenome, P] = crowdingDistance(fitness),
      cloneStrategy: CloneStrategy[P] = youngest[P]) = new Algorithm[GAGenome, P, Unit] {
    def initialState = Unit

    override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
      val challenge =
        for {
          c1 <- onRank(ranking)(pop)
          c2 <- onDiversity(diversity)(pop)
        } yield c1 and c2

      (random[Unit] lifts challenge).flatMap { challenged =>
        def fight = random[Unit] lifts tournament(challenged, pop)

        interleaveClones[GAGenome, P, Unit](
          newGenomes[P, Unit](fight, pop, operationExploration),
          fight.map(_.genome),
          lambda,
          cloneStrategy)
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
    niche: Niche[GAGenome, P, Int])(
      operationExploration: Double = 0.1,
      cloneStrategy: CloneStrategy[P] = youngest[P]) = new Algorithm[GAGenome, P, Unit] {
    def initialState = Unit

    implicit val nicheEqual = Equal.equal[Int](_ == _)

    override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
      val challenge = onRank(profileRanking(niche, fitness))(pop)

      (random[Unit] lifts challenge).flatMap { challenged =>
        def fight = random[Unit] lifts tournament(challenged, pop, size => math.round(math.log10(size).toInt))

        interleaveClones[GAGenome, P, Unit](
          newGenomes[P, Unit](fight, pop, operationExploration),
          fight.map(_.genome),
          lambda,
          cloneStrategy
        )
      }
    }

    override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] = {
      val p1 = merge(population, offspring)
      val p2 = applyCloneStrategy(p1, cloneStrategy)
      val p3 = removeNaN(p2, fitness)

      def keep(p: Pop) = State.gets { s: AlgorithmState[Unit] => keepBest(1, fitness)(p) }

      for { p4 <- nicheElitism(keep, p3, niche) } yield p4
    }
  }

}
