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

package org.openmole.tools.mgo.statistic

import org.openmole.tools.mgo.model.Goal
import org.openmole.tools.mgo.model.MultiGoal
import scala.collection.mutable.ArraySeq

object MinMax {
  
  def MinMax[T, P <: MultiGoal[T]](goals: Iterable[P]) = {
    val it = goals.iterator
    val first = it.next
    val size = first.goals.size
      
    val min = new ArraySeq[Goal[T]](size)
    val max = new ArraySeq[Goal[T]](size)
    
    for(i <- 0 until size) {
      min(i) = first.goals(i)
      max(i) = first.goals(i)
    }
        
    while(it.hasNext) {
      val current = it.next
            
      for(i <- 0 until size) {
        val currentVal = current.goals(i)
        
        if(currentVal.order.lt(currentVal.value, min(i).value)) {
          min(i) = currentVal
        } else if (currentVal.order.gt(currentVal.value, max(i).value)) {
          max(i) = currentVal
        }
      }
    }
    
    new MinMax(min, max)
  }
}

class MinMax[T, P <: MultiGoal[T]](val min: ArraySeq[T], val max: ArraySeq[T]) {
    
}
