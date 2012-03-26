/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.selection

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga.selection._

import fr.iscpif.mgo.tools._
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.GAGenome

object Distance {
 
  def crowding(indiv :IndexedSeq [Individual[_, GAFitness]]): IndexedSeq[Distance] = {  
    if (indiv.size <= 2) 
      indiv.map {
        i => new Distance {
          val distance = Double.PositiveInfinity
        }
      }        
    else {         
      class CrowdingInfo(val multiGoal: Individual[_, GAFitness], var crowding: Double)
      
      val crowding = indiv.map(new CrowdingInfo(_, 0.))

      // for each objective
      for (curDim <- 0 until indiv.head.fitness.fitness.size) {
        
        val curCrowding = crowding.sortBy(_.multiGoal.fitness.fitness(curDim))

        val firstCrowdingInfo = curCrowding.head
        val lastCrowdingInfo = curCrowding.last

        val first = firstCrowdingInfo.multiGoal.fitness
        val last = lastCrowdingInfo.multiGoal.fitness

        val min = first.fitness(curDim)
        val max = last.fitness(curDim)

        firstCrowdingInfo.crowding = Double.PositiveInfinity
        lastCrowdingInfo.crowding = Double.PositiveInfinity

        val maxMinusMin = max - min

        val itOpod = curCrowding.iterator
        var ptMinus1 = itOpod.next
        var pt = itOpod.next

        while (itOpod.hasNext) {
          val ptPlus1 = itOpod.next
          val distance =  (ptPlus1.multiGoal.fitness.fitness(curDim) - ptMinus1.multiGoal.fitness.fitness(curDim)) / maxMinusMin
          pt.crowding += distance.toDouble
  
          ptMinus1 = pt
          pt = ptPlus1
        }
      }

      crowding.map(c =>
        new Distance {
          val distance = c.crowding
        }
      )
    }
  }
}



trait Distance {
  def distance: Double
}
