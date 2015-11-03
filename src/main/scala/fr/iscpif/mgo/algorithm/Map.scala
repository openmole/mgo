/*
 * Copyright (C) 13/11/13 Romain Reuillon
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

trait Map[Point] <: Algorithm with GeneticAlgorithm with AllFunctions {

  case class MapState(hitMap: collection.Map[Point, Int])
  type STATE = MapState
  def initialState = MapState(collection.Map())

  implicit val hits = monocle.macros.Lenser[MapState](_.hitMap)

  implicit val fitness: Fitness[Double]
  implicit val niche: Niche[Point]
  implicit val pointEqual = Equal.equal[Point](_ == _)

  def cloneRate = 0.0
  implicit val mergeStrategy = youngest

  override def breeding(pop: Pop): State[AlgorithmState, Vector[G]] =
    onHitCount.apply(pop) flatMap { challenged =>
      val fight = tournament(challenged, pop, size => math.round(math.log10(size).toInt))
      for {
        op <- random.lifts(operation(pop))
        (crossover, mutation) = op
        newGenomes = breedGenomes(fight, crossover, mutation).map(_.map(clamp))
        genomes <- interleaveClones(newGenomes, fight.map(_.genome), cloneRate, lambda)
      } yield genomes.toVector
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- mergeClones(p1)
      p3 <- removeNaN(p2)
      p4 <- nicheElitism(keepBestRanked(1), p3)
    } yield p2

}

