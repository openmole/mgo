/*
 * Copyright (C) 2012 Romain Reuillon
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

package mgo.tools.metric

import cats.implicits._

/**
 * Crowding distance computation see Deb, K., Agrawal, S., Pratap, A. & Meyarivan, T.
 * A fast elitist non-dominated sorting genetic algorithm for multi-objective
 * optimization: NSGA-II. Lecture notes in computer science 1917, 849–858 (2000).
 */
object CrowdingDistance:

  /**
   * Compute the crowding distance
   *
   * @param data the set of point
   * @return the crowding distance of each point in the same order as the input
   * sequence
   */
  def computeCrowdingDistance(data: Seq[Vector[Double]]): Seq[Double] =
    case class Individual(objectives: Vector[Double], var distance: Double = 0.0)

    val n = data.length
    if n == 0
    then Seq.empty
    else
      val front = data.map(Individual(_, 0.0))
      val numObjectives = front.head.objectives.length

      for
        m <- 0 until numObjectives
      do
        val sorted = front.sortBy(_.objectives(m))

        sorted.head.distance = Double.PositiveInfinity
        sorted.last.distance = Double.PositiveInfinity

        val minObj = sorted.head.objectives(m)
        val maxObj = sorted.last.objectives(m)
        val range = maxObj - minObj

        if range != 0.0
        then
          for (i <- 1 until n - 1)
          do
            val prev = sorted(i - 1).objectives(m)
            val next = sorted(i + 1).objectives(m)
            sorted(i).distance += (next - prev) / range

      front.map(_.distance)

  def normalizedCrowdingDistance(data: Seq[Vector[Double]]): Seq[Double] =
    val distances = computeCrowdingDistance(data)
    val finiteMax = distances.filterNot(_.isPosInfinity).maxOption.getOrElse(1.0)

    distances.map:
      case d if d.isPosInfinity => 1.0
      case d => d / finiteMax


  def apply(data: Vector[Vector[Double]]): Vector[Double] =
    val distinctData = data.distinct
    val crowding = (distinctData zip computeCrowdingDistance(distinctData)).toMap
    data.map(crowding)



