/*
 * Copyright (C) 13/05/2014 Guillaume Chérel
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

trait PSE[Point] <: Algorithm with GeneticAlgorithm with AllFunctions {

  type STATE = collection.Map[Point, Int]
  def initialState = Map()

  implicit val hits = identityLens[STATE]
  implicit val niche: Niche[Point]
  implicit val pointEqual = Equal.equal[Point](_ == _)

  def cloneRate = 0.0
  implicit val mergeClones = youngest

  override def breeding(pop: Pop, lambda: Int): State[AlgorithmState, Vector[G]] =
    onHitCount.apply(pop) flatMap { challenged =>
      def fight = tournament(challenged, pop, size => math.round(math.log10(size).toInt))
      interleaveClones(newGenomes(fight, pop), fight.map(_.genome), lambda)
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- applyCloneStrategy(p1)
      p3 <- nicheElitism(keepRandom(1), p2)
    } yield p3

}