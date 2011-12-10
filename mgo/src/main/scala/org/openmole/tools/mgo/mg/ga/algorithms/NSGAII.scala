/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg.ga.algorithms

import java.util.Random
import org.openmole.tools.mgo.AbstractGenome
import org.openmole.tools.mgo.mg.ga.selection.BinaryTournamentNSGA2
import org.openmole.tools.mgo.model.MultiGoalLike
import org.openmole.tools.mgo.mg.filters._
import org.openmole.tools.mgo.mg.ga._
import org.openmole.tools.mgo._
import org.openmole.tools.mgo.domination._
import org.openmole.tools.mgo.mg._

//TODO : On ne peut pas deleguer la création/évaluation avec IndividualMGFactory[MG,G] 
//a operate(), car c'est une tache a part, cf workflow openmole...

class NSGAII[MG <: MultiGoalLike, G <: AbstractGenome, F <: GenomeFactory[G]]
            (mutationOperator:Mutation [G, F],selectionOperator:Selection [G,MG, F], crossoverOperator:CrossOver [G, F]) (implicit aprng: Random) {

    //type I = IndividualMG[G,MG] with IRanking with IDistance
    
    //On recupere une population evaluee d'individu
    def operate(P_t0:IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance], archive:PopulationMG[G,IndividualMG[G,MG] with IRanking with IDistance],sizeOfSampling:Int):(IndexedSeq[G],IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance]) = {
      
      // merge Q + Archive P
      val Q_total = new PopulationMG[G,IndividualMG[G,MG] with IRanking with IDistance](P_t0 ++ archive.individuals)
    
      // init empty new pop t+1
      var Q_partiel = IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance]()
        
      // Compute rank et distance pour tout les individus sur tout les fronts pour la pop t 
      val rankingFront = new RankingByNonDominatedFront[G,MG](new StrictDominant)
      rankingFront.operate(Q_total.individuals)

      val rankingDistance = new RankingByDistance[G,MG]
      rankingDistance.operate(Q_total.individuals)
      
      Q_total.individuals.map{ g=> println(g.multiGoal.toString +  " > distance => " + g.distances + " , rank => "+ g.rank)}
      
      // Pour chaque front, on rempli au mieux 
      var remain = sizeOfSampling
      var index = 0
      var front = Q_total.individuals.filter{_.rank == index}
      
      while ((remain > 0) && (remain >= front.size)) {
         
        // on ajoute le bloc
        Q_partiel = Q_partiel ++ front
        
        // on decremente remain
        remain = remain - front.size
        index += 1
        
        if (remain > 0){  
          front = Q_total.individuals.filter{_.rank == index}
        }
    
      }
    
     // Puis on ajoute les meilleurs sur le dernier front apres selection sur leur distance
     // car remain est < front
     if (remain > 0)
       {
         Q_partiel = Q_partiel ++ front.sortBy(_.distances).reverse.slice(0,remain)
         println("remain value = " + remain)
       }
     
    // Q <- Breed(P) avec Q nouvelle pop de genome à evaluer 
    // + la nouvelle population archive P 
    // On genere autant de genom que d'element dans l'ancienne archive et/ou Population de genome à évaluer
    // TODO : Verifier qu'il faut bien prendre au hasard, et non pas generer des couples uniques ...
    
    val popSize = archive.individuals.size
    val mattingPop = (selectionOperator.operate(Q_partiel,popSize)).map{_.genome}
    
    // On ne se sert pas de evolutionEngine ici, car on veut faire : crossover + mutation
    // On genere une nouvelle population a partir de la pop "matting" sous groupe issu du tournament 
    // CROSSOVER => 2 enfants a chaque fois ... donc pop / 2 forcement (a condition que la pop soit un multiple de 2 ... sinon bug)

    val offsprings:IndexedSeq[G] = (0 until popSize/2).map{x =>  crossoverOperator.operate(mattingPop)}.flatten
    
    // On execute la mutation sur la population apres crossover, et cela autant de fois qu'il ya d'individu
    val P_t1:IndexedSeq[G] = (0 until popSize).map{x => mutationOperator.operate(offsprings)}
    
    //println ("newPopulationAfterEvolve =" + newPopulationGenome.map{x => x.wrappedValues.toString}
      
    return (P_t1,Q_partiel)
             
    }


}
