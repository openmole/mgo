/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.algorithm

import java.util.Random
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.selection.BinaryTournamentNSGA2
import org.openmole.tools.mgo._
import org.openmole.tools.mgo.Individual._
import org.openmole.tools.mgo.ga.domination._
import org.openmole.tools.mgo.ga.selection.Distance
import org.openmole.tools.mgo.ga.selection.Ranking._
import org.openmole.tools.mgo.ga.selection.Ranking
import org.openmole.tools.mgo.tools.Math
import scala.annotation.tailrec

object NSGAII {
  implicit def individualsDecorator[FIT <: GAFitness, G](individuals: IndexedSeq[Individual[G, FIT]]) = new {
        
    def toIndividualsWithDistanceAndRanking(dominance: Dominant) = {
      val ranks = Ranking.rank(individuals, dominance)
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
  }  
  
}

import NSGAII._

class NSGAII[G <: GAGenome, F <: GAGenomeFactory[G]] (
  mutationOperator: Mutation [G, F],
  crossoverOperator: CrossOver [G, F]) {

  val dominance = new StrictDominant
  val selection = new BinaryTournamentNSGA2[Individual[G, _] with Distance with Ranking]
  
  def apply(population: IndexedSeq[Individual[G, GAFitness]] ,factory: F, evaluator: G => GAFitness, stopAfterSteady: Int)(implicit aprng: Random): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    
    def evolveUntilSteady(population: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking], steadyUntil: Int = 0): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
      if(steadyUntil >= stopAfterSteady) population
      else {
        val nextPop = evolve(population, factory, evaluator) 
        val evolved = !Math.allTheSameSorted(pareto(population).map{_.fitness.fitness}, pareto(nextPop).map{_.fitness.fitness})
        if(evolved) evolveUntilSteady(nextPop, 0)
        else evolveUntilSteady(nextPop, steadyUntil + 1)
      }
    }
    
    evolveUntilSteady(population.toIndividualsWithDistanceAndRanking(dominance))
  }
  
  def evolve(population: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking], factory: F, evaluator: G => GAFitness)(implicit aprng: Random): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    val offspring = generate(population, factory, population.size).map{ g => Individual(g, evaluator) }
    select(population ++ offspring, population.size)
  }
  
  def select(archive: IndexedSeq[Individual[G, GAFitness]], size: Int): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    val individuals = archive.toIndividualsWithDistanceAndRanking(dominance)
    
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
  
  /**
   * @param p_t0 : the offspring population, evaluated, result of of the actual generation - 1
   * @param archive : the population
   * @param sizeOfSampling : the quantity / limit of the new computed population of genome 
   */
  def generate(
    archive: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking],
    factory: F, offSpringSize: Int)(implicit aprng: Random): IndexedSeq[G] = {
    
    val mattingPop: IndexedSeq[G] = (selection.select(archive, archive.size)).map{_.genome}

    crossoverOperator.crossOver(mattingPop, factory)
    
    val offspring: IndexedSeq[G] = 
      Iterator.continually(
        crossoverOperator.crossOver(mattingPop, factory)
        match {
          case(i1, i2) => IndexedSeq(i1, i2)
        }).flatten.take(offSpringSize).toIndexedSeq
                                                               
    
    Iterator.continually(mutationOperator.mutate(offspring, factory)).take(offSpringSize).toIndexedSeq        
  }


}
