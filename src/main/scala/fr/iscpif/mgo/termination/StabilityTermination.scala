/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.termination

import math._
import fr.iscpif.mgo._

object StabilityTermination {
    case class StabilityState(val std: Double = Double.PositiveInfinity, val history: List[Double] = List.empty) {
    override def toString = std.toString
  }
}

import StabilityTermination._

trait StabilityTermination extends Termination {
  /// Size of the sliding window to evaluate the deviation
  def windowSize: Int
  
  /// Deviation considered has sufficiently low to stop
  def deviationEpsilon: Double
  
  type STATE = StabilityState
  
  def initialState(p: Population[G, MF]): STATE = new StabilityState
  
  def stability(state: StabilityState, newValue: Double) = {
    val newState = (newValue :: state.history).slice(0, windowSize)
    if(newState.size < windowSize) (false, new StabilityState(history = newState))
    else {
      val avg = newState.sum / newState.size
      val std = sqrt(newState.map{ v => pow(v - avg, 2) }.sum)
      (std < deviationEpsilon, new StabilityState(std, newState))
    } 
  }
}
