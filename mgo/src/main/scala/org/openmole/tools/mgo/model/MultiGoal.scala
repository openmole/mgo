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

import java.util.Random

object MultiGoal {
  
  @transient lazy val rand = new Random
  
  def buildInt(dim: Int, size: Int, max: Int): Array[MultiGoal] = {
    Array.fill(size)(buildInt(Array.fill(dim)( rand.nextInt(max) ): _*))
  }
  
  
  def buildInt(dim: Int, size: Int): Array[MultiGoal] = {
    Array.fill(size)(buildInt(Array.fill(dim)( rand.nextInt ): _*))
  }
  
  def buildInt(g: Int*): MultiGoal = {
    new MultiGoal(g.map{ e => new ToDouble{ override def toDouble = e.toDouble; override def toString = e.toString} }.toIndexedSeq)
  }
 
  def buildDouble(dim: Int, size: Int): Array[MultiGoal] = {
    Array.fill(size)(buildDouble(Array.fill(dim)( rand.nextDouble ): _*))
  }
  
  def buildDouble(g: Double*): MultiGoal = {
    new MultiGoal(g.map{ d => { new ToDouble { override def toDouble = d }}}.toIndexedSeq)
  }
  
  implicit def ordering[P <% MultiGoal] = new Ordering[P] {
    def compare(mg: P, other: P): Int = {
      val itMg = mg.goals.iterator
      val itOther = other.goals.iterator
      
      while(itMg.hasNext) {
        val curMg = itMg.next
        
        val comp = curMg.toDouble - itOther.next.toDouble
        if(comp != 0) return if(comp < 0) -1 else 1
      }
      0
    }
  }
  
  
  def orderOneDim[P <: MultiGoal](dim: Int, toOrder: IndexedSeq[P]): IndexedSeq[P] = {
    
    return toOrder.sortWith((left: P, right: P) => {
        val rightGoal = right.goals(dim)
        val leftGoal = left.goals(dim)

        leftGoal.toDouble < rightGoal.toDouble
        /* if(compare != 0) return compare
         return right.compareTo(left)*/
      })
      
  }
  
}

class MultiGoal(val goals: IndexedSeq[ToDouble]) {
  override def toString = goals.toString
  //override def toString = goals.map{_.toString}.reduceLeft { (l,r) => l + " " + r}
}


