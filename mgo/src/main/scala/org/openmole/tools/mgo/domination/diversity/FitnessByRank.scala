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

import org.openmole.tools.mgo.model.MultiGoal
import org.openmole.tools.mgo.model.MultiGoal._
import org.openmole.tools.mgo.paretoquick.ParetoQuick
import org.openmole.tools.mgo.domination.DominateMinimization._
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

//FIXME enable this class when ParetoQuick works
private object FitnessByRank {
//
//  def selectByFitnessAndCrowdingPareto[MG <: MultiGoal](toSelect: IndexedSeq[MG], resPopSize: Int): IndexedSeq[MG] = {
//    if(toSelect.isEmpty) toSelect
//    else selectByFitnessAndCrowdingPareto(toSelect, toSelect.iterator.next.goals.size, resPopSize)
//  }
//
//  def selectByFitnessAndCrowdingPareto[MG <: MultiGoal](toSelect: IndexedSeq[MG], dim: Int, resPopSize: Int): IndexedSeq[MG] =  {
//      
//    var all = new TreeSet[MG]
//    all ++= toSelect
//
//    val ret = new ArrayBuffer[MG](resPopSize)
//    var pareto = ParetoQuick.pareto(all, dim)
//    
//    while(resPopSize > ret.size + pareto.size) {
//      println(pareto.size)
//      ret ++= pareto
//      all --= pareto
//      pareto = ParetoQuick.pareto(all, dim)
//    }
//
//    for(elt <- Crowding.orderByDecreasingCrowding(pareto)) {
//      ret += elt._1
//      if(ret.size >= resPopSize) return ret
//    }
//  
//    ret
//  }
//
//  def selectByFitnessAndCrowding[MG <: MultiGoal](toSelect: IndexedSeq[MG], dim: Int, resPopSize: Int): IndexedSeq[MG] = {
//
//    if(toSelect.size <= resPopSize) return toSelect
//
//    val toSelectLevel = new Array[Int](toSelect.size)
// 
//    val v = Array.fill(toSelect.size)(new ListBuffer[Int])
//        
//    var curFront = new ListBuffer[Int]
//    // fronts: F[i] = indexess of the individuals contained in the ith front
//    // used to store the number of the first front
//    // int i = 0;
//
//    for (p <- 0 until toSelect.size) {          
//      for (q <- 0 until toSelect.size) {
//        if (isDominated(toSelect(p), toSelect(q))) {
//          toSelectLevel(p) = toSelectLevel(p) + 1
//          v(q) += p
//        }
//      }
//
//      // if no individual dominates p
//      if (toSelectLevel(p) == 0) curFront += p
//    }
//    
//    //curFront ++= (for(i <- 0 until v.size ; if(toSelectLevel(i) == 0)) yield i)
//    //println(v.map{_.toString})
//    val ret = new ArrayBuffer[MG](resPopSize)
//    // Collection<T> ret = new LinkedList<T>();
//
//
//    while(resPopSize >= ret.size + curFront.size) {
//       println(curFront.size)
//    
//      val nextFront = new ListBuffer[Int]
//      for(p <- curFront) {
//        ret += toSelect(p)
//
//        for(q <- v(p)) {
//          toSelectLevel(q) = toSelectLevel(q) - 1
//
//          if(toSelectLevel(q) == 0) nextFront += q
//        }
//      }
//      curFront = nextFront
//    }
//
//    
//    for(elt <- Crowding.orderByDecreasingCrowding(curFront.map{toSelect(_)}.toIndexedSeq)) {
//      ret += elt._1
//      if(ret.size >= resPopSize) return ret
//    }
//
//    return ret
//  }
}
