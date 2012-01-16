/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.selection

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga.GAFitness
import org.openmole.tools.mgo.ga.domination._
import org.openmole.tools.mgo.ga.selection._

object Ranking {
  
   def pareto[F <: GAFitness](individuals: IndexedSeq [Individual[_, F]], dominanceType: Dominant): IndexedSeq[Ranking]= { 
     /**
     * COMPUTE PARETO NON DOMINATED FRONT
     * TODO : Refactoring possible
     */ 
    
    class RankedIndiv(
      val individual: Individual[_, F],
      var rank: Int = 0)
      //var dominates: List[Int] = List.empty)
    // Reinit index et dominate
    val toRank = individuals.map{new RankedIndiv(_)}
    
    //var curFront = new ListBuffer[Int]
    // fronts: F[i] = indexess of the individuals contained in the ith front  
    // used to store the number of the first front

    // FIXME : 
    // Utilisable lorsqu'on veut mapper un individualMG vers un INdividualMG with IRanking'
    // mais pose probleme car on ne peut plus faire de cumulatif IndividualMG with IRanking, IDistance par exemple ..
    // val individualsRanked = individuals.map{x=> new IndividualMG(x.genome,x.multiGoal) with IRanking}
    
    for (p <- 0 until individuals.size) {          
      for (q <- 0 until individuals.size if q != p) {
        // p domine q ?
        val isDominated = dominanceType.isDominated(individuals(p).fitness, individuals(q).fitness)
        // si  q > p => isDominated = true
        if (isDominated) {
          // p est domine donc on augmente son rang ..
          toRank(p).rank += 1
          // On ajoute p a la liste de dominé de q (cf v(q))
          //toRank(q).dominates = individuals(q).dominate ::= p 
        }
      }
    }
    
    toRank.map{ r =>  
      new Ranking {
        val rank = r.rank
      }
    }
  
 }
}


trait Ranking {
  def rank: Int
}
