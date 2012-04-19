/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

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
import fr.iscpif.mgo.tools.Math
import ga._
import scala.annotation.tailrec

object NSGAII {

  trait NSGAIISigmaEvolution extends NSGAII with SigmaGAEvolution {
    type G = GAGenomeWithSigma
    type F = GAGenomeWithSigmaFactory
    type I = Individual[GAGenomeWithSigma, Fitness] with Diversity with Rank
    type FIT = Fitness
  }
  
  class NSGAIISigma(
    val distributionIndex: Double,
    val steadySince: Int,
    val archiveSize: Int,
    val evaluator: GAGenomeWithSigma => Fitness,
    val genomeSize: Int
  ) extends NSGAII with SigmaGAEvolution
       with MGBinaryTournamentSelection
       with FirstRankedSteadyTermination
       with NonDominatedSortingElitism
       with CoEvolvingSigmaValuesMutation
       with SBXBoundedCrossover 
       with CrowdingDistance
       with ParetoRanking
       with StrictDominance {
    type G = GAGenomeWithSigma
    type F = GAGenomeWithSigmaFactory
    type I = Individual[GAGenomeWithSigma, Fitness] with Diversity with Rank
    type FIT = Fitness
    def factory = new GAGenomeWithSigmaFactory {
      def size = genomeSize
    }
    
  }
  

}

// WORK NOTE (A ne pas effacer) :
/*
 Selection occurs two times in the evolutionary loop.First, in order to generate offsprings,
 parents must be selected from the current population (mating selection).Second, the new
 parent population has to be selected from the offspring and the previous parents (Environmental selection)
 A number of selection operators were proposed, which usually base the chance of selection of
 particular individuals on their fitness values or their rank in the population, respectively.



// Environmental selection
// How to prevent non-dominated solutions from being lost?
// Environmental selection is used to obtain a representative efficient set

 */

// @fixme Refaire un check sur Ranking

 trait NSGAII extends GAEvolution with MG with Archive with Elitism with DiversityMetric {

    type I <: Individual[G, FIT] with Diversity with Rank
  
    def archiveSize: Int
 
    implicit def indiv2IndivWithRankAndDistance(
      i: Iterable[Individual[G, Fitness]]
    ) = buildIndividualsWithDistanceAndRanking(i.toIndexedSeq)
 
    def buildIndividualsWithDistanceAndRanking(
      individuals: IndexedSeq[Individual[G, Fitness]]
    ): IndexedSeq[I] = {
      val ranks = rank(individuals)
      val distances = diversity(individuals)
      (individuals zip ranks zip distances) map {
        case ((i, iranking), idistance) =>
          new Individual[G, Fitness] with Diversity with Rank {
            val genome = i.genome
            val fitness = i.fitness
            val diversity = idistance.diversity
            val rank = iranking.rank
          }
      }
    }
  

    override def evolve(population: IndexedSeq[I])(implicit aprng: Random): IndexedSeq[I] = {

      val offspring = breed(
        population,
        population.size
      ).map {
        g => Individual(g, evaluator)
      }

      val archive = population ++ offspring

      //Elitisme strategy
      val individuals = buildIndividualsWithDistanceAndRanking(archive)
      elitism(individuals)
    }

    def breed(
      archive: IndexedSeq[I],
      offSpringSize: Int
    )(implicit aprng: Random): IndexedSeq[G] = {

      //Crossover sur matingPopulation puis mutation
      def breed(acc: List[G] = List.empty): List[G] = {
        if (acc.size >= offSpringSize) acc
        else {
          val newIndividuals = crossover(
            selection(archive).genome,
            selection(archive).genome,
            factory).map {
            mutate(_, factory)
          }.take(offSpringSize).toIndexedSeq
          breed(acc ++ newIndividuals)
        }
      }

      breed().toIndexedSeq
    }


  }
