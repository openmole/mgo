/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.model


import scala.actors.threadpool.AtomicInteger

object Individual {

  private var  currentId = new AtomicInteger
  
  def nextId = {
    currentId.incrementAndGet
  }
  
}

class Individual[GE, T](val genome: GE, val goals: IndexedSeq[Goal[T]]) extends MultiGoal[T] {
  
 // val id = Individual.nextId
  
 /* override def apply(dim: Int): Goal[T] = {
    goals.apply(dim)
  }*/

 /* override def size = {
    goal.size
  }

  override def apply(dim: Int): T = {
    goal.apply(dim)
  }

  override def compareTo(other: Individu[GE, T, GO]): Int = {
    val compare = goal.compareTo(other.goal)
    if(compare != 0) return compare
    id - other.id
  }

  override def toString: String = {
    val toString = new StringBuilder
    toString.append(goal.toString)
    toString.append(' ')
    toString.append(genome.toString)

    toString.toString
  }*/

}
