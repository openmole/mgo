/*
 * Copyright (C) 03/03/2014 Guillaume Chérel
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

package fr.iscpif.mgo.tools.metric

import fr.iscpif.mgo.tools.Lazy

/**
 * Modification of the crowding distance to avoid infinite values for the first and last
 * point in each domain. The crowding for the second and last element of each domain is instead
 * the distance between the first and second (or last and second but last).
 *
 * Crowding distance computation see Deb, K., Agrawal, S., Pratap, A. & Meyarivan, T.
 * A fast elitist non-dominated sorting genetic algorithm for multi-objective
 * optimization: NSGA-II. Lecture notes in computer science 1917, 849–858 (2000).
 */
object ClosedCrowdingDistance {

  /**
   * Compute the closed crowding distance
   *
   * @param data the set of point
   * @return the crowding distance of each point in the same order as the input
   * sequence
   */
  def apply(data: Seq[Seq[Double]]): Seq[Lazy[Double]] = {
    if (data.size < 2) data.map(d => Lazy(Double.PositiveInfinity))
    else {
      class CrowdingInfo(val d: Seq[Double], var crowding: Double = 0.0)

      val crowding = data.map(new CrowdingInfo(_))

      // for each objective
      for (curDim <- 0 until data.head.size) {

        val curCrowding = crowding.sortBy(_.d(curDim))

        val firstCrowdingInfo = curCrowding.head
        val secondCrowdingInfo = curCrowding(1)
        val secondButLastCrowdingInfo = curCrowding(curCrowding.size - 2)
        val lastCrowdingInfo = curCrowding.last

        val first = firstCrowdingInfo.d
        val second = secondCrowdingInfo.d
        val secondButLast = secondButLastCrowdingInfo.d
        val last = lastCrowdingInfo.d

        val min = first(curDim)
        val max = last(curDim)

        val maxMinusMin = max - min

        firstCrowdingInfo.crowding += (second(curDim) - first(curDim)) * 2 / maxMinusMin
        lastCrowdingInfo.crowding += (last(curDim) - secondButLast(curDim)) * 2 / maxMinusMin

        val itOpod = curCrowding.iterator
        var ptMinus1 = itOpod.next
        var pt = itOpod.next

        while (itOpod.hasNext) {
          val ptPlus1 = itOpod.next
          pt.crowding += (ptPlus1.d(curDim) - ptMinus1.d(curDim)) / maxMinusMin
          ptMinus1 = pt
          pt = ptPlus1
        }
      }

      crowding.map(c => Lazy(c.crowding))
    }
  }

  /**
   * Compute the closed crowding distance of the i-th point in the data points
   *
   * @param point the point
   * @param data the set of point
   * @return the closed crowding distance of the point relative to the data
   */

  def of(i: Int, data: Seq[Seq[Double]]): Lazy[Double] = {
    if (data.size < 2) Lazy(Double.PositiveInfinity)
    else {
      var crowdingOfI: Double = 0.0

      // for each objective
      for (curDim <- 0 until data.head.size) {

        val curSortedIdx = data.indices.sortBy(data(_)(curDim))

        val first = data(curSortedIdx.head)
        val last = data(curSortedIdx.last)

        val min = first(curDim)
        val max = last(curDim)

        val maxMinusMin = max - min

        if (i == curSortedIdx.head) crowdingOfI += (data(curSortedIdx(1))(curDim) - first(curDim)) * 2 / maxMinusMin
        else if (i == curSortedIdx.last) crowdingOfI += (last(curDim) - data(curSortedIdx(curSortedIdx.size - 2))(curDim)) * 2 / maxMinusMin
        else {
          var j = 1
          while (curSortedIdx(j) != i) {
            j += 1
          }
          val ptMinus1 = data(curSortedIdx(j - 1))
          val ptPlus1 = data(curSortedIdx(j + 1))
          crowdingOfI += (ptPlus1(curDim) - ptMinus1(curDim)) / maxMinusMin
        }
      }

      Lazy(crowdingOfI)
    }
  }
}