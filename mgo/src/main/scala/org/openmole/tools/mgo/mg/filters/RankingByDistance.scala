/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg.filters

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga.operators.diversity.Crowding._

import org.openmole.tools.mgo.mg._
import org.openmole.tools.mgo.model._
import org.openmole.tools.mgo.tools._

class RankingByDistance[G <: Genome, MG <: MultiGoalLike] extends Ranking[G,MG] {
  //(distance: Distance)
  type RankType = IndividualMG [G,MG] with IDistance
  
  override def operate(individuals :IndexedSeq [RankType]):IndexedSeq[RankType]={
  
    // TODO 1: Passer en parametre le type de distance a calculer, ici pour le moment on fait du crowding seulement...
    // TODO 2 : Peut mieux faire ici, le sort se passe bien car il prend un MG like, et IndividualMG est un MGLike, seuleument, 
    // ce mapping est tres moche, peut etre faudrait il creer un autre decorateur 
    // car en plus on a pas encore besoin de tri ici, on ne fait que l'initialisation de la valeur distances
    // 
    // Reinit distance
    individuals.map{_.distances = 0.0}
    
    for ((indiv, crowding) <- individuals.orderByDecreasingCrowding){
      //Mise à jour de l'attribut distance par la valeur du crowding
      indiv.distances = crowding
    }
    
    return individuals
  }
}