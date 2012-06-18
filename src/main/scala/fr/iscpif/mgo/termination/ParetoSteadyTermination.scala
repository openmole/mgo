/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.termination

import fr.iscpif.mgo._
import fr.iscpif.mgo.dominance.StrictDominance
import fr.iscpif.mgo.ranking.ParetoRanking
import fr.iscpif.mgo.ranking.Rank
import fr.iscpif.mgo.ranking.Rank._
import fr.iscpif.mgo.tools.Math

trait ParetoSteadyTermination extends Termination {
  self: Evolution with MG=>
  
  type STATE = Int
  
  def dominance = new StrictDominance{}
  
  def initialState = 0
  
  def steadySince: Int

  def terminated(a1: Population[G, I], a2: Population[G, I], step: STATE): (Boolean, STATE) = {
    def toRanked(a: Population[G, I]) = 
      (a zip ParetoRanking(a.evaluated, dominance)) map {
        case (i, r) =>
          new Individual[G] with Rank {
            val genome = i.genome
            val fitness = i.fitness
            val rank = r
          }
      }
    
    val newStep = if ( Math.allTheSame(firstRanked(toRanked(a1)).map {_.fitness.values},firstRanked(toRanked(a2)).map {_.fitness.values})) step + 1
    else  0
    
    (newStep >= steadySince, newStep)
  }

  
 
    
 
  
  
}