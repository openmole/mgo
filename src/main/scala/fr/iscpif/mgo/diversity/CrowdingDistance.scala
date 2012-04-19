/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._

trait CrowdingDistance extends DiversityMetric { this: Evolution =>

   def diversity(indiv :IndexedSeq [Individual[_, FIT]]): IndexedSeq[Diversity] = {  
    if (indiv.size <= 2) 
      indiv.map {
        i => new Diversity {
          val diversity = Double.PositiveInfinity
        }
      }        
    else {         
      class CrowdingInfo(val multiGoal: Individual[_, FIT], var crowding: Double)
      
      val crowding = indiv.map(new CrowdingInfo(_, 0.))

      // for each objective
      for (curDim <- 0 until indiv.head.fitness.values.size) {
        
        val curCrowding = crowding.sortBy(_.multiGoal.fitness.values(curDim))

        val firstCrowdingInfo = curCrowding.head
        val lastCrowdingInfo = curCrowding.last

        val first = firstCrowdingInfo.multiGoal.fitness
        val last = lastCrowdingInfo.multiGoal.fitness

        val min = first.values(curDim)
        val max = last.values(curDim)

        firstCrowdingInfo.crowding = Double.PositiveInfinity
        lastCrowdingInfo.crowding = Double.PositiveInfinity

        val maxMinusMin = max - min

        val itOpod = curCrowding.iterator
        var ptMinus1 = itOpod.next
        var pt = itOpod.next

        while (itOpod.hasNext) {
          val ptPlus1 = itOpod.next
          val distance =  (ptPlus1.multiGoal.fitness.values(curDim) - ptMinus1.multiGoal.fitness.values(curDim)) / maxMinusMin
          pt.crowding += distance.toDouble
  
          ptMinus1 = pt
          pt = ptPlus1
        }
      }

      crowding.map(c =>
        new Diversity {
          val diversity = c.crowding
        }
      )
    }
  }
}
