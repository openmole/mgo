/*
 * Copyright (C) 2011 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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