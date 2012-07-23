/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.termination

import fr.iscpif.mgo._
import math._

trait CrowdingStabilityTermination extends Termination {
  self: Evolution with CrowdingDistance {type MF <: Diversity } =>
  
  def windowSize: Int
  def stdEpsilon: Double
  
  case class CrowdingStabilityState(val history: List[Double] = List.empty, val std: Double = Double.PositiveInfinity)
  
  type STATE = CrowdingStabilityState
  
  def initialState: STATE = new CrowdingStabilityState
  
  def terminated(oldPop: Population[G, MF], newPop: Population[G, MF], terminationState: STATE) : (Boolean, STATE) = {
    val maxCrowding = newPop.map{_.metaFitness.diversity}.filter(_ != Double.PositiveInfinity).max
   
    val newState = (maxCrowding :: terminationState.history).slice(0, windowSize)
    if(newState.size < windowSize) (false, new CrowdingStabilityState(newState))
    else {
      val avg = newState.sum / newState.size
      val std = sqrt(newState.map{ v => pow(v - avg, 2) }.sum)
      (std < stdEpsilon, new CrowdingStabilityState(newState, std))
    } 
  }
  
}
