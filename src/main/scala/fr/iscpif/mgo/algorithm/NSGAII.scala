/*
 * Copyright (C) 22/05/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import scalaz._
import Scalaz._

trait NSGAII <: Algorithm with GeneticAlgorithm with AllFunctions {

  type STATE = Unit
  def initialState = Unit

  implicit def fitness: Fitness[Seq[Double]]
  implicit def ranking = paretoRanking()
  implicit def diversity = crowdingDistance
  implicit def mergeClones = youngest
  def cloneRate = 0.0

  override def breeding(pop: Pop): State[AlgorithmState, Vector[G]] =
    (onRank and onDiversity) (pop) flatMap { challenged =>
      def fight = tournament(challenged, pop)
      val newGenomes = breedGenomes(fight, crossover(pop), mutation(pop))
      interleaveClones(newGenomes.map(_.map(clamp)), fight.map(_.genome), cloneRate, lambda).map(_.toVector)
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- mergeClones(p1)
      p3 <- removeNaN(p2)
      p4 <- keepNonDominated(mu, p3)
    } yield age(p4)

}