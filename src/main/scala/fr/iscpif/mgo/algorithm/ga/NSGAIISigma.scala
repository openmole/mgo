/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.algorithm.ga

import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.algorithm._
import java.util.Random
import fr.iscpif.mgo.crossover._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.selection._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo._
import fr.iscpif.mgo.termination._

object NSGAIISigma {
  
  trait NSGAIISigmaBasic extends NSGAIISigma
                     with MGBinaryTournamentSelection
                     with CounterTermination
                     with NonDominatedSortingElitism
                     with CoEvolvingSigmaValuesMutation
                     with SBXBoundedCrossover 
                     with CrowdingDistance
                     with ParetoCrowdingRanking
                     with EpsilonDominance
  
}


trait NSGAIISigma extends NSGAII 
                     with SigmaGAEvolution {
  type G = GAGenomeWithSigma
  type F = GAGenomeWithSigmaFactory
  type FIT = Fitness
    
  def genomeSize: Int
  
  def factory = new GAGenomeWithSigmaFactory {
    def size = genomeSize
  }
    
}