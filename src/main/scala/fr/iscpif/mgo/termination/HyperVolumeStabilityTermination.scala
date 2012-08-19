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

trait HyperVolumeStabilityTermination extends Termination with ReferencePoint with Dominance with RankModifier {

  def windowSize: Int

  def deviationEpsilon: Double

  case class HyperVolumeStabilityState(val std: Double = Double.PositiveInfinity, val history: List[Double] = List.empty) {
    override def toString = std.toString
  }

  type STATE = HyperVolumeStabilityState

  def initialState(p: Population[G, MF]): STATE = new HyperVolumeStabilityState

  def terminated(population: Population[G, MF], terminationState: STATE): (Boolean, STATE) = {
    val rankMax = population.map{_.metaFitness.rank()}.max
    val front = population.filter(_.metaFitness.rank() == rankMax).map{_.fitness.values}
    val hv = Hypervolume(front, referencePoint(front), this)

    val newState = (hv :: terminationState.history).slice(0, windowSize)
    if (newState.size < windowSize) (false, new HyperVolumeStabilityState(history = newState))
    else {
      val avg = newState.sum / newState.size
      val std = sqrt(newState.map {
        v => pow(v - avg, 2)
      }.sum)
      (std < deviationEpsilon, new HyperVolumeStabilityState(std, newState))
    }
  }
}
