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

package object algorithm {

  def nsga2[P](
    mu: Int,
    fitness: Fitness[GAGenome, P, Seq[Double]],
    operationExploration: Double = 0.1)(
      ranking: Ranking[GAGenome, P] = paretoRanking(fitness = fitness),
      diversity: Diversity[GAGenome, P] = crowdingDistance(fitness),
      cloneStrategy: CloneStrategy[P] = youngest[P]) = {

    new Algorithm[GAGenome, P, Unit] {
      def initialState = Unit

      override def breeding(pop: Pop, lambda: Int): State[AlgorithmState[Unit], Vector[GAGenome]] = {
        val challenge =
          for {
            c1 <- onRank(ranking)(pop)
            c2 <- onDiversity(diversity)(pop)
          } yield c1 and c2

        (random[Unit] lifts challenge).flatMap { challenged =>
          def fight = tournament[GAGenome, P, Unit, (Lazy[Int], Lazy[Double])](challenged, pop)

          val interleaved: State[AlgorithmState[Unit], Vector[GAGenome]] = interleaveClones[GAGenome, P, Unit](
            newGenomes[P, Unit](fight, pop, operationExploration),
            fight.map(_.genome),
            lambda,
            cloneStrategy)

          interleaved
        }
      }

      override def elitism(population: Pop, offspring: Pop): State[AlgorithmState[Unit], Pop] =
        for {
          p1 <- merge(population, offspring)
          p2 <- applyCloneStrategy(p1, cloneStrategy)
          p3 <- removeNaN(p2, fitness)
          p4 <- keepNonDominated(mu, p3, ranking, diversity)
        } yield p4
    }
  }

}
