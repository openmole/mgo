/*
 * Copyright (C) 2010 reuillon
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

package org.openmole.tools.mgo.domination.diversity

import org.openmole.tools.mgo.model.MultiGoalLike
import org.openmole.tools.mgo.model.MultiGoal
import org.openmole.tools.mgo.model.MultiGoal._

object Crowding {
  
  implicit def withCrowdingDecorator[MG <: MultiGoalLike](goals: Iterable[MG]) = new WithCrowdingDecorator(goals)
  
  
  class WithCrowdingDecorator[MG <: MultiGoalLike](goals: Iterable[MG]) {
  
    def orderByDecreasingCrowding = {

      if (goals.size <= 2) {
        goals.map{(_, Double.PositiveInfinity)}.toIndexedSeq
      } else {         
        class CrowdingInfo(val multiGoal: MG, var crowding: Double) extends MultiGoal(multiGoal.goals)
      
        val crowding = goals.map( new CrowdingInfo(_, 0.) ).toSeq

        // for each objective
        for (curDim <- 0 until goals.head.goals.size) {
        
          val curCrowding = crowding.orderOneDim(curDim)

          val firstCrowdingInfo = curCrowding.head
          val lastCrowdingInfo = curCrowding.last

          val first = firstCrowdingInfo.multiGoal
          val last = lastCrowdingInfo.multiGoal

          val min = first.goals(curDim).toDouble
          val max = last.goals(curDim).toDouble

          firstCrowdingInfo.crowding = Double.PositiveInfinity
          lastCrowdingInfo.crowding = Double.PositiveInfinity

          val maxMinusMin = max - min

          val itOpod = curCrowding.iterator
          var ptMinus1 = itOpod.next
          var pt = itOpod.next

          while (itOpod.hasNext) {
            val ptPlus1 = itOpod.next
            val distance =  (ptPlus1.goals(curDim).toDouble - ptMinus1.goals(curDim).toDouble) / maxMinusMin
            pt.crowding += distance.toDouble
  
            ptMinus1 = pt
            pt = ptPlus1
          }
        }

        crowding.sortWith((a, b) => a.crowding < b.crowding).map( elt => (elt.multiGoal, elt.crowding))
      }

    }
  }
}
