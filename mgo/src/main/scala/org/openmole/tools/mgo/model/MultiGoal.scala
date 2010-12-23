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

package org.openmole.tools.mgo.model


trait MultiGoal[T] {
  def goals: IndexedSeq[Goal[T]]
  override def toString = goals.toString 
}

object MultiGoal {
  
  implicit def ordering[T, P <% MultiGoal[T]] = new Ordering[P] {
    def compare(mg: P, other: P): Int = {
      val itMg = mg.goals.iterator
      val itOther = other.goals.iterator
      
      while(itMg.hasNext) {
        val curMg = itMg.next
        
        val comp = curMg.order.compare(curMg.value, itOther.next.value)
        if(comp != 0) return 0
      }
      0
    }
  }
  
  
  def orderOneDim[T, P <: MultiGoal[T]](dim: Int, toOrder: IndexedSeq[P]): IndexedSeq[P] = {
    
    return toOrder.sortWith((left: P, right: P) => {
          val rightGoal = right.goals(dim)
          val leftGoal = left.goals(dim)
          
          val order = rightGoal.order
          import order._
          
          leftGoal.value < rightGoal.value
         /* if(compare != 0) return compare
          return right.compareTo(left)*/
        })
      
  }
  
}