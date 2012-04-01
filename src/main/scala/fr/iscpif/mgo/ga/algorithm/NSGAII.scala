/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import java.util.Random
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ga.operators.crossover.SBXBoundedCrossover
import fr.iscpif.mgo.ga.operators.mutation.CoEvolvingSigmaValuesMutation
import fr.iscpif.mgo.ga.selection.BinaryTournamentNSGA2
import fr.iscpif.mgo._
import fr.iscpif.mgo.CrossOver
import fr.iscpif.mgo.Individual._
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection.Distance
import fr.iscpif.mgo.ga.selection.Ranking._
import fr.iscpif.mgo.ga.selection.ParetoRank
import fr.iscpif.mgo.ga.selection.Ranking
import fr.iscpif.mgo.ga.selection.Rank
import fr.iscpif.mgo.tools.Math
import scala.annotation.tailrec

object NSGAII {
  
  def buildIndividualsWithDistanceAndRanking[G <: GAGenome, FIT <: GAFitness](
    individuals: IndexedSeq[Individual[G, FIT]],
    dominance: Dominant,
    rank: Rank) = {
    val ranks = rank(individuals, dominance)
    val distances = Distance.crowding(individuals)
    (individuals zip ranks zip distances) map { 
      case ((i, iranking), idistance) =>  
        new Individual[G, FIT] with Distance with Ranking {
          val genome = i.genome
          val fitness = i.fitness
          val distance = idistance.distance
          val rank = iranking.rank
        }
    }
  }
  
  def elitism[G <: GAGenome](
    archive: IndexedSeq[Individual[G, GAFitness]], 
    size: Int,
    rank: Rank = new ParetoRank
  )(implicit dominance: Dominant): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    val individuals = buildIndividualsWithDistanceAndRanking(archive, dominance, rank)
    
    if(individuals.size < size) individuals
    else {
      val fronts = individuals.groupBy(_.rank).toList.sortBy(_._1).map{_._2}
      
      @tailrec def addFronts[I](fronts: List[IndexedSeq[I]], acc: List[I]): (IndexedSeq[I], List[I]) = {
        if(acc.size + fronts.head.size < size) addFronts(fronts.tail, acc ++ fronts.head)
        else (fronts.headOption.getOrElse(IndexedSeq.empty), acc)
      }
    
      val (lastFront, selected) = addFronts(fronts, List.empty)

      (if (selected.size < size) selected ++ lastFront.sortBy(_.distance).reverse.slice(0, size - selected.size) else selected).toIndexedSeq
    }
  }
  
  def breed[G <: GAGenome, F <: GAGenomeFactory[G]](
    archive: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking],
    factory: F, 
    offSpringSize: Int,
    selection: Selection[Individual[G, _] with Distance with Ranking],
    mutationOperator: Mutation [G, F],
    crossoverOperator: CrossOver [G, F]
  )(implicit aprng: Random): IndexedSeq[G] = {
    
    def breed(acc: List[G] = List.empty): List[G] = {
      if(acc.size >= offSpringSize) acc
      else {
        val newIndividuals = crossoverOperator(selection(archive).genome, selection(archive).genome, factory)
        breed(acc ++ newIndividuals)
      }
    }

    breed().toIndexedSeq
  }

  
  def sigma(
    sbxDistributionIndex: Double,
    dominance: Dominant = new StrictDominant,
    rank: Rank = new ParetoRank,
    selection: Selection[Individual[GAGenomeWithSigma, _] with Distance with Ranking] = new BinaryTournamentNSGA2[Individual[GAGenomeWithSigma, _] with Distance with Ranking]
  ) = {
    val _dominance = dominance
    val _selection = selection
    val _rank = rank
    
    new NSGAII[GAGenomeWithSigma, GAGenomeWithSigmaFactory] {
      val mutationOperator = new CoEvolvingSigmaValuesMutation[GAGenomeWithSigma, GAGenomeWithSigmaFactory]
      val crossoverOperator = new SBXBoundedCrossover[GAGenomeWithSigma, GAGenomeWithSigmaFactory](sbxDistributionIndex)
      val dominance = _dominance
      val selection = _selection
      val rank = _rank
      
    }
  }
    
  
}

import NSGAII._

abstract class NSGAII[G <: GAGenome, F <: GAGenomeFactory[G]] {
 
  def mutationOperator: Mutation [G, F]
  def crossoverOperator: CrossOver [G, F]
  def dominance: Dominant
  def selection: Selection[Individual[G, _] with Distance with Ranking]
  def rank: Rank
  def steady(oldPop: IndexedSeq[Individual[G, GAFitness]], newPop: IndexedSeq[Individual[G, GAFitness]]) =
    sameFirstRanked(oldPop, newPop, dominance)
  
  def apply(population: IndexedSeq[Individual[G, GAFitness]] ,factory: F, evaluator: G => GAFitness, stopAfterSteady: Int)(implicit aprng: Random): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    
    @tailrec def evolveUntilSteady(population: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking], steadyUntil: Int = 0): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
      //println(steadyUntil)
      //population.foreach{i => println(i.rank + " " + i.genome)}
      if(steadyUntil >= stopAfterSteady) population
      else {
        val nextPop = evolve(population, factory, evaluator)
        if(!steady(population, nextPop)) evolveUntilSteady(nextPop, 0)
        else evolveUntilSteady(nextPop, steadyUntil + 1)
      }
    }
    
    evolveUntilSteady(buildIndividualsWithDistanceAndRanking(population, dominance, rank))
  }
  
  def evolve(population: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking], factory: F, evaluator: G => GAFitness)(implicit aprng: Random): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    val offspring = breed(
      population, 
      factory,
      population.size,
      selection,
      mutationOperator,
      crossoverOperator
    ).map{ g => Individual(g, evaluator) }
    elitism(population ++ offspring, population.size, rank)(dominance)
  }

  


}
