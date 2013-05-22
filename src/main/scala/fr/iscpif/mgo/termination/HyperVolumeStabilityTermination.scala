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

/**
 * Terminates when the hypervolume contribution of the last ranked individuals
 * in the population has been stabilized.
 */
trait HyperVolumeStabilityTermination extends Termination with ReferencePoint with Dominance with RankModifier with StabilityTermination {

  type F <: MGFitness

  override def terminated(population: => Population[G, P, F, MF], terminationState: STATE): (Boolean, STATE) = {
    val p = population
    val rankMax = p.map { _.metaFitness.rank() }.max
    val front = p.filter(_.metaFitness.rank() == rankMax).map { _.toIndividual.fitness.values }
    val hv = Hypervolume(front, referencePoint, this)
    stability(terminationState, hv)
  }
}
