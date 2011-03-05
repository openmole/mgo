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
import org.openmole.tools.mgo.model.MultiGoal._
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import org.openmole.tools.mgo.model.ToDouble._
import scala.collection.IndexedSeqOptimized
import scala.math._

///Don't use it, it is not working
private object ParetoQuick {

 
  class Front[P <: MultiGoal](val multiGoal: P, var front: Int) extends MultiGoal(multiGoal.goals) {
    def updateFront(otherFront: Int) = { front = max(front, otherFront) }
  }
  
  def pareto[P <: MultiGoal](points: Iterable[P]): IndexedSeq[P] = {
    points.headOption match {
      case None => IndexedSeq.empty[P]
      case Some(p) => pareto(points, p.goals.size)
    }
  }
    
  def pareto[P <: MultiGoal](points: Iterable[P], dim: Int): IndexedSeq[P] = {
    dc(points.toIndexedSeq, dim - 1)
  }   

  
  def helperA[P <: MultiGoal](s: IndexedSeq[Front[P]], m: Int, doSort: Boolean = true): Unit = {
    if(s.size > 1) {
      if(s.size == 2) {
        if(mdomA(s(0).multiGoal, s(1).multiGoal, m)) s(1).updateFront(s(0).front + 1)
        else if(mdomA(s(1).multiGoal, s(0).multiGoal, m)) s(0).updateFront(s(1).front + 1)
      } else {
        if(m == 2) {
          //val ret = new ListBuffer[Front[P]]
          
          var stairs = new TreeMap[Double, Int]
          val sortedS = orderFirstDim(s)
          
          var si = 0
          stairs += sortedS(si).goals(1).toDouble -> sortedS(si).front
          
          var lasto0 = sortedS(si).goals(0)
          var lasto1 = sortedS(si).goals(1)
          
          var lastFront = sortedS(si).front
          
          si += 1
          
          
          while(si < sortedS.size) {
            val siGoal = sortedS(si).goals
            
            val stairKeys = stairs.keys.toIndexedSeq
            var pos = 0
            
            
            while(pos < stairKeys.size && stairKeys(pos) < siGoal(1)) { pos += 1}
            if(pos < stairKeys.size) pos -= 1
            
            if(stairKeys(pos) > siGoal(1) && pos > 0) pos -= 1
            
            if( siGoal(1) > stairKeys(pos) ||
               (siGoal(1) == stairKeys(pos) && (siGoal(0) != lasto0 || siGoal(1) != lasto1))) {
              sortedS(si).updateFront(stairs(stairKeys(pos)) + 1)
            } else {
              if(siGoal(0) == lasto0 && siGoal(1) == lasto1) {
                sortedS(si).updateFront(lastFront)
              }
            }
            
            if(stairKeys(pos) <= siGoal(1) && stairs(stairKeys(pos)) < sortedS(si).front
               || stairKeys(pos) > siGoal(1)) {
              stairs += siGoal(1).toDouble -> sortedS(si).front
              stairs = cleanupStairs(stairs, siGoal(1))
              lasto0 = siGoal(0)
              lasto1 = siGoal(1)
              lastFront = sortedS(si).front
            }
            si += 1
            //if(pos >= stairs.size) pos -= 1
          }
        } else {
          val sortedS =  if(doSort) {
            orderOneDim(m-1, s)
          } else s
          
          if(sortedS(0).goals(m-1) != sortedS(sortedS.size - 1).goals(m - 1)) {
            var cutinx = sortedS.size / 2
            if(sortedS(cutinx).goals(m-1) != sortedS(sortedS.size - 1).goals(m - 1)) {
              while(sortedS(cutinx + 1).goals(m-1) == sortedS(cutinx).goals(m - 1)) cutinx += 1
              cutinx += 1
            } else {
              while(sortedS(cutinx - 1).goals(m-1) == sortedS(cutinx).goals(m - 1)) cutinx -= 1
            }
            
            val l = sortedS.slice(0, cutinx)
            val h = sortedS.slice(cutinx, sortedS.size)
            
            helperA(l, m, false)
            helperB(l, h, m - 1, true)
            helperA(h, m, false)
          } else{
            helperA[P](sortedS, m - 1, true)
          }
        }
      } 
    } 
  }
  
  
  def helperB[P <: MultiGoal](l: IndexedSeq[Front[P]], h: IndexedSeq[Front[P]], m: Int, doSort: Boolean = true): Unit = {
    if(l.size != 0 && h.size != 0) {
      if(l.size == 1) {
        h.filter( mdomB(l.head, _, m)).map{elt => elt.updateFront(l.head.front + 1)}
      } else {
        if(h.size == 1) {
          l.filter( mdomB(h.head, _, m)).map{elt => elt.updateFront(h.head.front + 1)}
        } else {
          if(m == 2) {
            val sortedL = orderFirstDim(l)
            val sortedH = orderFirstDim(h)
            var stairs = new TreeMap[Double, Int]
            
            var hi = sortedH.findIndexOf( cmp2D(_, sortedL.head) )
            if(hi == -1) hi = sortedH.size
            var li = 0
           
            def curL = sortedL(li)
            def curH = sortedH(hi)
            
            stairs += curL.goals(1).toDouble -> curL.front
            li += 1
            
            while(hi < sortedH.size) {
              while(li < sortedL.size && !cmp2D(curH, curL)) {
                var pos = 0
                //lower_bound
              }
            }
          } else {
          
          }
        }
      }
  
    }
  }

  
  def cleanupStairs(stairs: TreeMap[Double, Int], xval: Double): TreeMap[Double, Int] = {
    val frontNum = stairs(xval)
    var ret = TreeMap.empty[Double, Int]
    ret ++= (stairs.view.dropWhile( _._1 <= xval).filter(_._2 <= frontNum))
    ret
  }
  
  
  def mdomA[P <: MultiGoal](s: P, s2: P, m: Int): Boolean = {
    var better = false
    s.goals.zip(s2.goals).slice(0, m).foreach {
      case(objS, objS2) => {
          if(objS.toDouble > objS2.toDouble) return false
          if(objS.toDouble < objS2.toDouble) better = true
        }
    }
    return better
  }
  
  
  def mdomB[P <: MultiGoal](s: P, s2: P, m: Int): Boolean = {
    s.goals.zip(s2.goals).slice(0, m).foreach {
      case(objS, objS2) => if(objS.toDouble > objS2.toDouble) return false
    }
    return true
  }
  
  def dc[P <: MultiGoal](z: IndexedSeq[P], dim: Int): IndexedSeq[P] = {
    //Logger.getLogger(ParetoQuick.getClass.getName).info(z.toString)
    
    if (dim == 1) return archivePareto2D(z) //DC - 1    
    if (z.size <= 2) return sc(z) //DC - 2
    
    val splited = MultiGoal.orderOneDim(dim, z).splitAt(z.size/2) //DC - 3
    val xx = dc(splited._2, dim) //DC - 4
    val yy = dc(splited._1, dim) //DC - 4
    val xxxPrime = marry(xx, yy, dim - 1) //DC - 5 + 6
    
    //Logger.getLogger(ParetoQuick.getClass.getName).info("xx = " + xx)
    //Logger.getLogger(ParetoQuick.getClass.getName).info("yy = " + yy)
    //Logger.getLogger(ParetoQuick.getClass.getName).info("res = " + (yy ++ xxxPrime))
    
    yy ++ xxxPrime //DC - 7
  } 
  
  def sc[P <: MultiGoal](v: IndexedSeq[P]): IndexedSeq[P] = {

    if(v.isEmpty) return IndexedSeq.empty
    if(v.size == 1) return v
    val it = v.iterator

    val p1 = it.next
    val p2 = it.next
    // Logger.getLogger(ParetoQuick.getClass.getName).info("Domin: " + p1.toString + " " + p2.toString)

    return DominateMinimization.dominated(p1, p2) match {       
      case LEFT => IndexedSeq(p2)
      case RIGHT => IndexedSeq(p1)
      case NONE => v
        //case _ => IndexedSeq.empty
    }
      
    //Logger.getLogger(ParetoQuick.getClass.getName).info("Domin res: " + ret.toString)
  }
  
  
//  def pareto[T, P <: MultiGoal[T]](points: Iterable[P], dim: Int): IndexedSeq[P] = {
//
//    val curDim = dim - 1
//
//    if (points.isEmpty) {
//      return IndexedSeq.empty
//    }
//    if (curDim == 0) {
//      val paretoPoint = points.head.goals
//      val order = paretoPoint(curDim).order
//      import order._
//      
//      val ret = new ArrayBuffer[P]
//      
//      for(current <- points.tail) {
//        if(order.lt(current.goals(curDim).value, paretoPoint(curDim).value)) {
//          ret.clear
//          ret += current
//        }
//        if(current.goals(curDim).value == paretoPoint(curDim).value) ret += current
//      }
//      
//      return ret.toIndexedSeq[P]
//    }
//
//
//    val toProceed = new Queue[IndexedSeq[P]]
//   
//    val it = points.iterator
//
//    while (it.hasNext) {
//      val f = it.next
//
//      val vect = if(it.hasNext) {
//        ArraySeq[P](f, it.next)
//      } else ArraySeq[P](f)
//
//      toProceed += vect
//    }
//    
//    var archive = toProceed.dequeue
//
//    while (!toProceed.isEmpty) {
//      val archive2 = eliminer[T,P](toProceed.dequeue, curDim)
//  
//      val merged = new ArrayBuffer[P](archive.size + archive2.size)
//      merged ++= eliminer[T,P](archive, curDim)
//      merged ++= /*eliminer[T,P](*/archive2//, curDim)
//
//      toProceed += merged.toIndexedSeq[P]
//
//      archive = toProceed.dequeue
//    }
//
//    return eliminer[T,P](archive, curDim)
//  }
//
// 
//  def testSolvable[T, P <: MultiGoal[T]](v: IndexedSeq[P], curDim: Int): Option[IndexedSeq[P]] = {
//    if (v.size == 2) {
//      val it = v.iterator
//
//      val p1 = it.next
//      val p2 = it.next
//      // Logger.getLogger(ParetoQuick.getClass.getName).info("Domin: " + p1.toString + " " + p2.toString)
//
//      return DominateMinimization.dominated(p1, p2) match {       
//        case LEFT => Some(IndexedSeq(p2))
//        case RIGHT => Some(IndexedSeq(p1))
//        case NONE => Some(IndexedSeq(p1, p2))
//          //case _ => IndexedSeq.empty
//      }
//      
//      //Logger.getLogger(ParetoQuick.getClass.getName).info("Domin res: " + ret.toString)
//    }
//   
//    None
//  }
//  
//  
//  def test2D[T, P <: MultiGoal[T]](orderedV: IndexedSeq[P], curDim: Int): Option[IndexedSeq[P]] = {
// 
//    if (curDim == 1) {
//      val archive = new ListBuffer[P]
//      archivePareto2D[T,P](orderedV, archive)
//      return Some(archive.toIndexedSeq[P])
//    }
//    
//    None
//  }
//  
//
//  def eliminer[T, P <: MultiGoal[T]](v: IndexedSeq[P], curDim: Int): IndexedSeq[P] = {
//    if (v.isEmpty) return ArraySeq.empty[P]
//    if(v.size == 1) return v.toIndexedSeq
//
//    testSolvable[T,P](v, curDim) match {
//      case Some(value) => return value
//      case None =>
//    }
//    
//    val orderedV = MultiGoal.orderOneDim[T,P](curDim, v)
// 
//    test2D[T,P](v, curDim) match {
//      case Some(value) => return value
//      case None =>
//    }
//    
//    val half = v.size / 2
//    
//    val y = orderedV.slice(0, half)
//    val x = orderedV.slice(half, orderedV.size)
//    val married = marry[T, P](x, y, curDim - 1)
//
//    val union = new ArrayBuffer[P](married.size + y.size)
//    union ++= married
//    union ++= y
//    
//    return union
//    
////    //Logger.getLogger(ParetoQuick.getClass.getName).info("OrderedV " + orderedV.toString)
////
////    
////    
////
////  
////
////
////    
////    
////    
////    
////    val vTagged = new ArrayBuffer[Tagged[T,P]](v.size)
////
////    val it = orderedV.iterator
////    
////    for (i <- 0 until half) vTagged += new Tagged(it.next, Tag.A)
////    for (i <- half until v.size) vTagged += new Tagged(it.next, Tag.B)
////    
////    //Logger.getLogger(ParetoQuick.getClass.getName).info("Tagged " + vTagged.map( t => (t.multiGoal, t.tag) ).toString)
//// 
////    
////    val orderedVTagged = MultiGoal.orderOneDim[T,Tagged[T,P]](curDim - 1, vTagged)
////
////    //Logger.getLogger(ParetoQuick.getClass.getName).info("orderedVTagged " + orderedVTagged.map( t => (t.multiGoal, t.tag) ).toString)
////
////    
////    val testElP = new ListBuffer[P]
////    val testElNP = new ListBuffer[P]    
////    
////    /*  Logger.getLogger(ParetoQuick.getClass.getName).info("EP: " + orderedVTagged.slice(0, vTagged.size / 2).map(_.goals).map( _.map(_.value)).toString)
////     eliminerP(orderedVTagged.slice(0, vTagged.size / 2), testElP)
////    
////     Logger.getLogger(ParetoQuick.getClass.getName).info("ENP: " + orderedVTagged.slice(vTagged.size / 2, vTagged.size).map(_.goals).map( _.map(_.value)).toString)
////     eliminerNP(orderedVTagged.slice(vTagged.size / 2, vTagged.size), testElP)
////     Logger.getLogger(ParetoQuick.getClass.getName).info(testElP.toString)*/
////  
////    val procV1 = eliminerP(orderedVTagged.slice(0, half), archive)
////    val procV2 = eliminerNP(orderedVTagged.slice(half, vTagged.size), archive)
////
////    val newV = new ArrayBuffer[P](procV1.size + procV2.size)
////
////    newV ++= procV1
////    newV ++= procV2
////
////    archive ++= eliminer[T,P](newV, curDim - 1)
////    
////    archive.toIndexedSeq
//  }
  
  
  def archivePareto2D[P <: MultiGoal](v: IndexedSeq[P]): IndexedSeq[P] = {

    //Logger.getLogger(ParetoQuick.getClass.getName).info(vect.toString)

    if (v.isEmpty) return IndexedSeq.empty

    val orderedV = MultiGoal.orderOneDim[P](1, v)
    
    val archive = new ListBuffer[P]
    
    val it = v.iterator

    var elt = it.next
    archive += elt

    var min = elt.goals(0)

    while (it.hasNext) {
      elt = it.next
      val goal = elt.goals(0)
    
      
      if ( goal.toDouble <= min.toDouble ) { /* || (second.getComparable(1).compareTo(elt.getComparable(1)) == 0)) {*/
//        Logger.getLogger(ParetoQuick.getClass.getName).info(elt.toString)

        min = elt.goals(0)
        archive += elt
        // second = elt;
      } 
    }
    archive.toIndexedSeq[P]
  }
//
//  def eliminerNP[T, P <: MultiGoal[T]](points: Iterable[Tagged[T,P]], archive: ListBuffer[P]): Seq[P] = {
//    val ret = new ListBuffer[P]
//
//    //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
//    //l'objectif de cette classe est d'???limner les Point du 2i???me vecteur qui sont domin???s par le 1ier vecteur
//    // les points qui restent du 2i???me vecteur dans le 1er vecteur sont envoy???s dans l'archive pareto et ???liminer du vecteur en cour
//
//
//    for (p <- points) {
//      if (p.tag == Tag.B) {
//        ret += p.multiGoal
//      } else {
//        archive += p.multiGoal
//      }
//    }
//
//    
//    //   Logger.getLogger(ParetoQuick.getClass.getName).info("NP input " + points.map( t => (t.multiGoal, t.tag) ).toString)
//
//    //   Logger.getLogger(ParetoQuick.getClass.getName).info("NP output " + ret.toString)
//
//    
//    ret
//  }
//
//  def eliminerP[T, P <: MultiGoal[T]](points: Iterable[Tagged[T, P]], archive: ListBuffer[P]): Seq[P] = {
//    val ret = new ListBuffer[P]
//    val retA = new ListBuffer[P]
//
//    //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
//    //l'objectif est de cette classe est d'???liminer les Point du 1i???me vecteur qui se trouvent dans le 2ieme vecteur
//
//    for (p <- points) {
//      if (p.tag == Tag.B) {
//        ret += p.multiGoal
//      } else {
//        retA += p.multiGoal
//      }
//    }
//
//    for (val p <- ret) {
//      if (pointNotDominatedByList[T,P](p, retA)) {
//        archive += p
//      }
//
//    }
//
//    //   Logger.getLogger(ParetoQuick.getClass.getName).info("P input " + points.map( t => (t.multiGoal, t.tag) ).toString)
//
//    //   Logger.getLogger(ParetoQuick.getClass.getName).info("P output " + retA.toString)
//
//    
//    return retA
//  }
//
  def pointNotDominatedByList[P <: MultiGoal](p: P, points: IndexedSeq[P]): Boolean = {
    for (p1 <- points) {
      if (DominateMinimization.isDominated(p, p1)) return false
    }
    return true
  }

  
  def notDominatedByPoint[P <: MultiGoal](p: P, points: IndexedSeq[P]): IndexedSeq[P] = {
    (for(p1 <- points ; if(!DominateMinimization.isDominated(p1, p))) yield p1).toIndexedSeq
  }
  
  
  def marry[P <: MultiGoal](x: IndexedSeq[P], y: IndexedSeq[P], curDim: Int): IndexedSeq[P] = {
    /*if (x.isEmpty) return y
     if (y.isEmpty) return x*/

    if(x.size == 1) return if(pointNotDominatedByList(x.head, y)) IndexedSeq(x.head) else IndexedSeq.empty
    if(y.size == 1) return notDominatedByPoint(y.head, x)
    
    /*if(x.size == 2 && y.size == 2) {
     paretoPointList[T,P](x(1), paretoPointList[T,P](x(0), y))
     }*/
    //Logger.getLogger(ParetoQuick.getClass.getName).info("dim " + curDim)
    //Logger.getLogger(ParetoQuick.getClass.getName).info("x "  + x)
    //Logger.getLogger(ParetoQuick.getClass.getName).info("y "  + y)
    // Logger.getLogger(ParetoQuick.getClass.getName).info("marry2D "  + marry2D[T,P](x,y))
    
    if(curDim == 1) return marry2D(x,y)
    
    val orderedX = MultiGoal.orderOneDim(curDim, x)
    val orderedY = MultiGoal.orderOneDim(curDim, y)
    
    val middleOfX = x.size / 2
    val middleOfY = y.size / 2
    
    val x1 = orderedX.slice(0, middleOfX)
    val x2 = orderedX.slice(middleOfX, x.size)
    
    val y1 = orderedY.slice(0, middleOfY)
    val y2 = orderedY.slice(middleOfY, y.size)
    
    val xp1 = marry(x1,y1, curDim)
    val xp2 = marry(x2,y2, curDim)
    
    val curDimP = curDim - 1
    val xs2 = marry(xp2, y1, curDimP)
    
    return xp1 ++ xs2
  }
  
  def marry2D[P <: MultiGoal](x: IndexedSeq[P], y: IndexedSeq[P]): IndexedSeq[P] = {
    //Logger.getLogger(ParetoQuick.getClass.getName).info("x " + x)
    //Logger.getLogger(ParetoQuick.getClass.getName).info("y " + y)
    //Marry2D - 1
    val marryTagged = new ArrayBuffer[Tagged[P]](x.size + y.size) 
    for (elt <- x) marryTagged += new Tagged(elt, Tag.A)
    for (elt <- y) marryTagged += new Tagged(elt, Tag.B)
    
    //Marry2D - 2
    val sortedMarryTagged = MultiGoal.orderOneDim(1, marryTagged)
    var w = sortedMarryTagged.head

    
    val xPrime = new ListBuffer[P]
       
    //Logger.getLogger(ParetoQuick.getClass.getName).info("Tagged "  + marryTagged.map{ e => ""+e.multiGoal + " " + e.tag} )
    
    for(elt <- sortedMarryTagged) {
      
      //Logger.getLogger(ParetoQuick.getClass.getName).info("Elt "  + elt)
      //Logger.getLogger(ParetoQuick.getClass.getName).info("W "  + w)
      if(elt.goals(0).toDouble <= w.goals(0).toDouble || w.goals(1).toDouble == elt.goals(1).toDouble || (w.tag == Tag.A && !DominateMinimization.isDominated(elt, w))) {
        //if (w.tag == Tag.A && !DominateMinimization.isDominated(elt, w)) println("w " + w + " elt" + elt)
        if(elt.tag == Tag.A) xPrime += elt.multiGoal
        else w = elt
      }
      
    }
    //Logger.getLogger(ParetoQuick.getClass.getName).info(xPrime.toString)
    return xPrime.toIndexedSeq 
  }
    
}
