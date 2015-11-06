/*
 * Copyright (C) 22/06/13 Romain Reuillon
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

trait Profile <: Algorithm with GeneticAlgorithm with AllFunctions with NicheFunctions {

  type STATE = Unit
  def initialState = Unit

  implicit val fitness: Fitness[Double]
  implicit val niche: Niche[Int]
  implicit val nicheEqual = Equal.equal[Int](_ == _)
  implicit val mergeClones = youngest
  def cloneRate = 0.0

  override def breeding(pop: Pop, lambda: Int): State[AlgorithmState, Vector[G]] =
    onRank(profileRanking).apply(pop) flatMap { challenged =>
      def fight = tournament(challenged, pop, size => math.round(math.log10(size).toInt))
      interleaveClones(newGenomes(fight, pop), fight.map(_.genome), lambda)
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- applyCloneStrategy(p1)
      p3 <- removeNaN(p2)
      p4 <- nicheElitism(keepBestRanked(1), p3)
    } yield p4

}