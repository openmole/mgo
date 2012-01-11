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

package org.openmole.tools.mgo.mg.ga.selection

import org.openmole.tools.mgo.ga._

import org.openmole.tools.mgo.mg._
import org.openmole.tools.mgo.model.MultiGoal
import org.openmole.tools.mgo.model.MultiGoal._
import org.openmole.tools.mgo.model.MultiGoalLike

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.domination._
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import java.util.Random
import org.openmole.tools.mgo.tools.Random._

//PROBLEME DE TYPE ICI
//TODO : IDEM AVEC INDIVIDUALMG et INDIVIDUAL ... IL NOUS FAUT UN SUPERTYPE ?
class BinaryTournamentNSGA2[G <: Genome,   MG <: MultiGoalLike, F <: GenomeFactory [G]] (implicit val factory : F) 
extends Selection[G,MG,F] {
  //TODO : probleme avec la dominance passé ici, elle sert au calcul initial, 
  //et pas a la comparaison, donc peut etre il faudra differencie leur utilisation ?
  //(dominanceType: Dominant)
  
  /**
   * Renvoie une une liste d'individu pour le mating
   * avec un binary tournament sur le rank et la distance.
   */
  
  override def operate (individuals: IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance],
                        numberGenerated:Int) (implicit aprng : Random) : IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance]={
    return (0 until numberGenerated).map{ x => binaryTournament(individuals.random(aprng), individuals.random(aprng))}.toIndexedSeq
  } 
  
  def binaryTournament(individual1:IndividualMG[G,MG] with IRanking with IDistance, individual2:IndividualMG[G,MG] with IRanking with IDistance)(implicit aprng: Random):IndividualMG[G,MG] with IRanking with IDistance = {
    if (individual1.rank < individual2.rank){
      return individual1
    }
    else if(individual1.rank > individual2.rank) {
      return individual2
    }
    else if (individual1.distances > individual2.distances){
      return individual1
    }
    else if (individual2.distances > individual1.distances){
      return individual2
    }else{
      if (aprng.nextDouble() < 0.5){
        return individual1
      }else{
        return individual2
      }
    }
  }
}
