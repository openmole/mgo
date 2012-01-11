/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg.filters

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.domination._
import org.openmole.tools.mgo.mg._
import org.openmole.tools.mgo.model._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

class RankingByNonDominatedFront[G <: Genome, MG <: MultiGoalLike] (dominanceType: Dominant)
  extends Ranking[G,MG] {
    
type RankType = IndividualMG [G,MG] with IRanking

//https://forge.iscpif.fr/projects/mgo/repository/revisions/dev/entry/mgo/src/main/scala/org/openmole/tools/mgo/selection/FitnessByRank.scala

override def operate(individuals :IndexedSeq [RankType]):IndexedSeq[RankType] = {
   
     /**
     * COMPUTE PARETO NON DOMINATED FRONT
     * TODO : Refactoring possible
     */ 
    
    // Reinit index et dominate
    individuals.map{_.rank = 0}
    individuals.map{_.dominate = IndexedSeq.empty}
    
    var curFront = new ListBuffer[Int]
    // fronts: F[i] = indexess of the individuals contained in the ith front  
    // used to store the number of the first front

    // FIXME : 
    // Utilisable lorsqu'on veut mapper un individualMG vers un INdividualMG with IRanking'
    // mais pose probleme car on ne peut plus faire de cumulatif IndividualMG with IRanking, IDistance par exemple ..
    // val individualsRanked = individuals.map{x=> new IndividualMG(x.genome,x.multiGoal) with IRanking}
    
    for (p <- 0 until individuals.size) {          
      for (q <- 0 until individuals.size if q != p) {
        // p domine q ?
        val isDominated = dominanceType.isDominated(individuals(p), individuals(q))
        // si  q > p => isDominated = true
        if (isDominated) {
          // p est domine donc on augmente son rang ..
          individuals(p).rank = individuals(p).rank + 1
          // On ajoute p a la liste de dominé de q (cf v(q))
          individuals(q).dominate = individuals(q).dominate :+ p 
        }
      }

      // si on veut renvoyer le front courant plutot qu'une liste
      // if no individual dominates p
      //if (individuals(p).rank == 0) {
      //  curFront += p
      //}
    }
    
  return individuals
  
 }
}
