/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import java.util.Random
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ga.selection.BinaryTournamentNSGA2
import fr.iscpif.mgo._
import fr.iscpif.mgo.CrossOver
import fr.iscpif.mgo.Individual._
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection.Distance
import fr.iscpif.mgo.ga.selection.Ranking._
import fr.iscpif.mgo.ga.selection.Ranking
import fr.iscpif.mgo.tools.Math
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
  
  
  def elitism[G <: GAGenome](
    archive: IndexedSeq[Individual[G, GAFitness]], 
    size: Int)(implicit dominance: Dominant): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
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
  
  def breed[G <: GAGenome, F <: GAGenomeFactory[G]](
    archive: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking],
    factory: F, 
    offSpringSize: Int,
    selection: Selection[Individual[G, _] with Distance with Ranking],
    mutationOperator: Mutation [G, F],
    crossoverOperator: CrossOver [G, F]
  )(implicit aprng: Random): IndexedSeq[G] = {
    
    val mattingPop: IndexedSeq[G] = (selection.select(archive, archive.size)).map{_.genome}
    
    val offspring = 
      Iterator.continually(
        crossoverOperator.crossOver(mattingPop, factory)
        match {
          case(i1, i2) => IndexedSeq(i1, i2)
        }).flatten.take(offSpringSize).toIndexedSeq
                                                               
    
    Iterator.continually(mutationOperator.mutate(offspring, factory)).take(offSpringSize).toIndexedSeq        
  }

}

import NSGAII._

class NSGAII[G <: GAGenome, F <: GAGenomeFactory[G]] (
  mutationOperator: Mutation [G, F],
  crossoverOperator: CrossOver [G, F]) {

  implicit val dominance = new StrictDominant
  val selection = new BinaryTournamentNSGA2[Individual[G, _] with Distance with Ranking]
  
  def apply(population: IndexedSeq[Individual[G, GAFitness]] ,factory: F, evaluator: G => GAFitness, stopAfterSteady: Int)(implicit aprng: Random): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
    
    @tailrec def evolveUntilSteady(population: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking], steadyUntil: Int = 0): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {
      if(steadyUntil >= stopAfterSteady) population
      else {
        val nextPop = evolve(population, factory, evaluator) 
        if(!samePareto(population, nextPop)) evolveUntilSteady(nextPop, 0)
        else evolveUntilSteady(nextPop, steadyUntil + 1)
      }
    }
    
    evolveUntilSteady(population.toIndividualsWithDistanceAndRanking(dominance))
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
    elitism(population ++ offspring, population.size)
  }

  


}
