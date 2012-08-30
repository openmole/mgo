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

package fr.iscpif.mgo.metric


import fr.iscpif.mgo.tools.Lazy

/**
 * Crowding distance computation see Deb, K., Agrawal, S., Pratap, A. & Meyarivan, T. 
 * A fast elitist non-dominated sorting genetic algorithm for multi-objective 
 * optimization: NSGA-II. Lecture notes in computer science 1917, 849–858 (2000).
 */
object CrowdingDistance {
  
  /**
   * Compute the crowding distance
   * 
   * @param data the set of point
   * @return the crowding distance of each point in the same order as the input
   * sequence
   */
  def apply(data: IndexedSeq[Seq[Double]]): IndexedSeq[Lazy[Double]] = {
    if (data.size <= 2) data.map(d => Lazy(Double.PositiveInfinity))
    else {         
      class CrowdingInfo(val d: Seq[Double], var crowding: Double)
      
      val crowding = data.map(new CrowdingInfo(_, 0.))

      // for each objective
      for (curDim <- 0 until data.head.size) {
        
        val curCrowding = crowding.sortBy(_.d(curDim))

        val firstCrowdingInfo = curCrowding.head
        val lastCrowdingInfo = curCrowding.last

        val first = firstCrowdingInfo.d
        val last = lastCrowdingInfo.d

        val min = first(curDim)
        val max = last(curDim)

        firstCrowdingInfo.crowding = Double.PositiveInfinity
        lastCrowdingInfo.crowding = Double.PositiveInfinity

        val maxMinusMin = max - min

        val itOpod = curCrowding.iterator
        var ptMinus1 = itOpod.next
        var pt = itOpod.next

        while (itOpod.hasNext) {
          val ptPlus1 = itOpod.next
          val distance =  (ptPlus1.d(curDim) - ptMinus1.d(curDim))  / maxMinusMin
          pt.crowding += distance.toDouble
  
          ptMinus1 = pt
          pt = ptPlus1
        }
      }

      crowding.map(c => Lazy(c.crowding))
    }
  }
  
}
