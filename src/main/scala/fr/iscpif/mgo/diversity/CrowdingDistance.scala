/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._

object CrowdingDistance {
  
  def apply(data: IndexedSeq[Seq[Double]]): IndexedSeq[Double] = {
    if (data.size <= 2) data.map(d => Double.PositiveInfinity)
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
          val distance =  (ptPlus1.d(curDim) - ptMinus1.d(curDim)) / maxMinusMin
          pt.crowding += distance.toDouble
  
          ptMinus1 = pt
          pt = ptPlus1
        }
      }

      crowding.map(_.crowding)
    }
  }
  
}


trait CrowdingDistance extends DiversityMetric { this: Evolution =>

   def diversity(evaluated: IndexedSeq[(G, Fitness)]): IndexedSeq[Double] =   
      CrowdingDistance(evaluated.map{_._2.values})
    
}
