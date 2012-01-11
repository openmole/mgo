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

//@todo : On ne peut pas deleguer la création/évaluation avec IndividualMGFactory[MG,G] 
//a operate(), car c'est une tache a part, cf workflow openmole...

class NSGAII[MG <: MultiGoalLike, G <: AbstractGenome, F <: GenomeFactory[G]]
            (mutationOperator:Mutation [G, F],selectionOperator:Selection [G,MG, F], 
             crossoverOperator:CrossOver [G, F]) (implicit aprng: Random) {

    //type I = IndividualMG[G,MG] with IRanking with IDistance
    
    /**
     * @param p_t0 : the offspring population, evaluated, result of of the actual generation - 1
     * @param archive : the population
     * @param sizeOfSampling : the quantity / limit of the new computed population of genome 
     */
    
    def operate(P_t0:IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance], 
                archive:PopulationMG[G,IndividualMG[G,MG] with IRanking with IDistance],
                sizeOfSampling:Int):(IndexedSeq[G],IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance]) = {
      
      // Equivalence :
      // Q = P_t0
      // P = archive
      
      // Q_total = merge Q + P
      // Attention, au premier tour execute par operate on ne fait pas de breed de la population avant son evaluation
      val Q_total = new PopulationMG[G,IndividualMG[G,MG] with IRanking with IDistance](P_t0 ++ archive.individuals)
    
      // QPartiel = selected population into Q_Total
      // Here, we init QPartiel empty 
      var Q_partiel = IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance]()
        
      // Compute rank et distance for each individuals for each pareto front
      val rankingFront = new RankingByNonDominatedFront[G,MG](new StrictDominant)
      rankingFront.operate(Q_total.individuals)

      val rankingDistance = new RankingByDistance[G,MG]
      rankingDistance.operate(Q_total.individuals)
      
      //println value for each individuals of Q_Total
      Q_total.individuals.map{ g=> println(g.multiGoal.toString +  " > distance => " + g.distances + " , rank => "+ g.rank)}
      
      // For each front we try to insert individuals into Q_partiel
      var remain = sizeOfSampling
      var index = 0
      var front = Q_total.individuals.filter{_.rank == index}
      
      // fill the new Q_partiel with max limit equal to remain value
      while ((remain > 0) && (remain >= front.size)) {
         
        // adding all the bloc in one time
        Q_partiel = Q_partiel ++ front
        
        // decrement remain
        remain = remain - front.size
        index += 1
        
        if (remain > 0){  
          front = Q_total.individuals.filter{_.rank == index}
        }
    
      }
    
     // A the end, if we have a front larger than computed remain value, 
     // we add only the best individuals, based on distance value

     if (remain > 0)
       {
         Q_partiel = Q_partiel ++ front.sortBy(_.distances).reverse.slice(0,remain)
         println("remain value = " + remain)
       }
    
    /**
    * BREED STEP
    * Q <- Breed(P) with Q equal to new genome population to evaluate
    * and P the new matting population
    */

    // @todo : Verifier qu'il faut bien prendre au hasard, et non pas generer des couples uniques ...
    // number of individual in the last size of archive  
    val popSize = archive.individuals.size
    
    //mattingPop compare couple of individuals taken in Q_partiel and return one selected genome for each individual of last archive(see popSize) 
    val mattingPop = (selectionOperator.operate(Q_partiel,popSize)).map{_.genome}
    
    // On ne se sert pas de evolutionEngine ici, car on veut faire : crossover + mutation
    // On genere une nouvelle population a partir de la pop "matting" sous groupe issu du tournament 
    // CROSSOVER => 2 enfants a chaque fois ... donc pop / 2 forcement (a condition que la pop soit un multiple de 2 ... sinon bug)
    
    // crossover on matting pop
    
    val offsprings:IndexedSeq[G] = (0 until popSize/2).map{x =>  crossoverOperator.operate(mattingPop)}.flatten
    
    // mutation on matting pop
    val P_t1:IndexedSeq[G] = (0 until popSize).map{x => mutationOperator.operate(offsprings)}
    
    //println ("newPopulationAfterEvolve =" + newPopulationGenome.map{x => x.wrappedValues.toString}
    
    //P_t1 is equal the new population of genome to evaluate
    //Q_partiel is equal to the new archive of individual, based on selection
    return (P_t1,Q_partiel)
             
    }


}
