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
import ToDouble._

object MultiGoal {
  
  //@transient lazy val rand = new Random
  
  def buildInt(dim: Int, size: Int, max: Int)(implicit rand: Random): Array[MultiGoal] = {
    Array.fill(size)(buildInt(Array.fill(dim)( rand.nextInt(max) ): _*))
  }
  
  def buildInt(dim: Int, size: Int)(implicit rand: Random): Array[MultiGoal] = {
    Array.fill(size)(buildInt(Array.fill(dim)( rand.nextInt ): _*))
  }
  
  def buildInt(g: Int*): MultiGoal = {
    new MultiGoal(g.map{ e => new ToDouble{ override def toDouble = e.toDouble; override def toString = e.toString} }.toIndexedSeq)
  }
 
  def buildDouble(dim: Int, size: Int)(implicit rand: Random): Array[MultiGoal] = {
    Array.fill(size)(buildDouble(Array.fill(dim)( rand.nextDouble ): _*))
  }
  
  def buildDouble(g: Double*): MultiGoal = {
    new MultiGoal(g.map{ d => { new ToDouble { override def toDouble = d }}}.toIndexedSeq)
  }
  
  implicit def ordering[P <: MultiGoalLike] = new Ordering[P] {
    def compare(mg: P, other: P): Int = {
      val itMg = mg.goals.iterator
      val itOther = other.goals.iterator
      
      while(itMg.hasNext) {
        val curMg = itMg.next
        
        val comp = curMg - itOther.next
        if(comp != 0) return if(comp < 0) -1 else 1
      }
      0
    }
  }
  
  def orderFirstDim[P <: MultiGoalLike](toOrder: IndexedSeq[P]): IndexedSeq[P] = {
    return toOrder.sortWith(cmp2D)
  }
  
  
  def cmp2D[P <: MultiGoalLike](left: P, right: P) = {
    val rightGoals = right.goals
    val leftGoals = left.goals
        
    leftGoals(0) < rightGoals(0) || (leftGoals(0) == rightGoals(0) && leftGoals(1) < rightGoals(1))
  }
  
  implicit def MultiGoalIterable2MultiGoalIterableDecorator[P <: MultiGoalLike](toOrder: Seq[P]) = new MultiGoalIterableDecorator(toOrder)
  
  class MultiGoalIterableDecorator[P <: MultiGoalLike](toOrder: Seq[P]) {
    def orderOneDim(dim: Int): Iterable[P] = {
    
      return toOrder.sortWith((left: P, right: P) => {
          val rightGoal = right.goals(dim)
          val leftGoal = left.goals(dim)

          leftGoal < rightGoal
          /* if(compare != 0) return compare
           return right.compareTo(left)*/
        })
      
    }
  }
  
}

class MultiGoal(val goals: IndexedSeq[ToDouble]) extends MultiGoalLike {
  def this(g: ToDouble*) = this((g.toIndexedSeq))
}

