/*
 *  Copyright (C) 2010 Romain Reuillon <romain.reuillon at openmole.org>
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


package org.openmole.tools.mgo.paretoquick

import java.util.logging.Logger
import org.openmole.tools.mgo.domination.DominateMinimization
import org.openmole.tools.mgo.domination.DominateType._
import org.openmole.tools.mgo.model.MultiGoal
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object ParetoQuick {

  
  def pareto[T, P <: MultiGoal[T]](points: Iterable[P]): IndexedSeq[P] = {
    points.headOption match {
      case None => IndexedSeq.empty[P]
      case Some(p) => pareto[T,P](points, p.goals.size)
    }
  }
    
  def pareto[T, P <: MultiGoal[T]](points: Iterable[P], dim: Int): IndexedSeq[P] = {

    val curDim = dim - 1

    if (points.isEmpty) {
      return IndexedSeq.empty
    }
    if (curDim == 0) {
      val paretoPoint = points.head.goals
      val order = paretoPoint(curDim).order
      import order._
      
      val ret = new ArrayBuffer[P]
      
     for(current <- points.tail) {
        if(order.lt(current.goals(curDim).value, paretoPoint(curDim).value)) {
          ret.clear
          ret += current
        }
        if(current.goals(curDim).value == paretoPoint(curDim).value) ret += current
      }
      
      return ret.toIndexedSeq[P]
    }


    val toProceed = new Queue[IndexedSeq[P]]
   
    val it = points.iterator

    while (it.hasNext) {
      val f = it.next

      val vect = if(it.hasNext) {
        ArraySeq[P](f, it.next)
      } else ArraySeq[P](f)

      Logger.getLogger(ParetoQuick.getClass.getName).info("Decoup: " + vect.toString)

      
      toProceed += vect
    }
    
    var archive = toProceed.dequeue

    while (!toProceed.isEmpty) {
      val archive2 = eliminer[T,P](toProceed.dequeue, curDim)
  
      val merged = new ArrayBuffer[P](archive.size + archive2.size)
      merged appendAll eliminer[T,P](archive, curDim)
      merged appendAll eliminer[T,P](archive2, curDim)

      toProceed += merged.toIndexedSeq[P]

      archive = toProceed.dequeue
    }

    return eliminer[T,P](archive, curDim)
  }

 

  def eliminer[T, P <: MultiGoal[T]](v: IndexedSeq[P], curDim: Int): IndexedSeq[P] = {
    if (v.isEmpty) {
      return ArraySeq.empty[P]
    }
   
    if (v.size == 2) {
      val it = v.iterator

      val p1 = it.next
      val p2 = it.next
    // Logger.getLogger(ParetoQuick.getClass.getName).info("Domin: " + p1.toString + " " + p2.toString)

      DominateMinimization.dominated(p1, p2) match {       
        case LEFT => IndexedSeq(p2)
        case RIGHT => IndexedSeq(p1)
        case NONE => IndexedSeq(p1, p2)
        case _ => IndexedSeq.empty
      }
      
      //Logger.getLogger(ParetoQuick.getClass.getName).info("Domin res: " + ret.toString)
    }
   
    val archive = new ListBuffer[P]

    if (curDim == 1) {
      archivePareto2D[T,P](MultiGoal.orderOneDim[T,P](curDim, v), archive)
      return archive.toIndexedSeq[P]
    }

    val half = v.size / 2

    val it = MultiGoal.orderOneDim[T,P](curDim, v).iterator

    val vTagged = new ArrayBuffer[Tagged[T,P]](v.size)

    for (i <- 0 until half) vTagged += new Tagged(it.next, Tag.A)
    for (i <- half until v.size) vTagged += new Tagged(it.next, Tag.B)

    val orderedVTagged = MultiGoal.orderOneDim[T,Tagged[T,P]](curDim - 1, vTagged)

    val testElP = new ListBuffer[P]
    val testElNP = new ListBuffer[P]    
    
  /*  Logger.getLogger(ParetoQuick.getClass.getName).info("EP: " + orderedVTagged.slice(0, vTagged.size / 2).map(_.goals).map( _.map(_.value)).toString)
    eliminerP(orderedVTagged.slice(0, vTagged.size / 2), testElP)
    Logger.getLogger(ParetoQuick.getClass.getName).info(testElP.toString)
    
    Logger.getLogger(ParetoQuick.getClass.getName).info("ENP: " + orderedVTagged.slice(vTagged.size / 2, vTagged.size).map(_.goals).map( _.map(_.value)).toString)
    eliminerNP(orderedVTagged.slice(vTagged.size / 2, vTagged.size), testElP)
    Logger.getLogger(ParetoQuick.getClass.getName).info(testElP.toString)*/
  
    val procV1 = eliminerP(orderedVTagged.slice(0, half), archive)
    val procV2 = eliminerNP(orderedVTagged.slice(half, vTagged.size), archive)

    val newV = new ArrayBuffer[P](procV1.size + procV2.size)

    newV ++= procV1
    newV ++= procV2

    archive ++= eliminer[T,P](newV, curDim - 1)

    archive.toIndexedSeq
  }
  
  
  def archivePareto2D[T, P <: MultiGoal[T]](vect: Seq[P], archive: ListBuffer[P]) {

    //Logger.getLogger(ParetoQuick.getClass.getName).info(vect.toString)

    
    if (vect.isEmpty) return

    val it = vect.iterator

    var elt = it.next
    archive += elt

    var min = elt.goals(0)

    while (it.hasNext) {
      elt = it.next
      val goal = elt.goals(0)
      val order = goal.order
      
      if ( order.lteq(goal.value, min.value) ) { /* || (second.getComparable(1).compareTo(elt.getComparable(1)) == 0)) {*/
//        Logger.getLogger(ParetoQuick.getClass.getName).info(elt.toString)

        min = elt.goals(0)
        archive += elt
        // second = elt;
      } 
    }
  }

  def eliminerNP[T, P <: MultiGoal[T]](points: Iterable[Tagged[T,P]], archive: ListBuffer[P]): Seq[P] = {
    val ret = new ListBuffer[P]

    //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
    //l'objectif de cette classe est d'???limner les Point du 2i???me vecteur qui sont domin???s par le 1ier vecteur
    // les points qui restent du 2i???me vecteur dans le 1er vecteur sont envoy???s dans l'archive pareto et ???liminer du vecteur en cour


    for (p <- points) {
      if (p.tag == Tag.B) {
        ret += p.multiGoal
      } else {
        archive += p.multiGoal
      }
    }

    ret
  }

  def eliminerP[T, P <: MultiGoal[T]](points: Iterable[Tagged[T, P]], archive: ListBuffer[P]): Seq[P] = {
    val ret = new ListBuffer[P]
    val retA = new ListBuffer[P]

    //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
    //l'objectif est de cette classe est d'???liminer les Point du 1i???me vecteur qui se trouvent dans le 2ieme vecteur

    for (p <- points) {
      if (p.tag == Tag.B) {
        ret += p.multiGoal
      } else {
        retA += p.multiGoal
      }
    }

    for (val p <- ret) {
      if (dominatePointList[T,P](p, retA)) {
        archive += p
      }

    }

    return retA
  }

  def dominatePointList[T, P <: MultiGoal[T]](p: P, points: Iterable[P]): Boolean = {

    for (p1 <- points) {
      if (DominateMinimization.isDominated(p, p1)) {
        return false
      }
    }
    return true

  }
}
