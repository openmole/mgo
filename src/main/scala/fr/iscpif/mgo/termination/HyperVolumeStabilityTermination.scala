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
import fr.iscpif.mgo.modifier.RankMF

/**
 * Terminates when the hypervolume contribution of the last ranked individuals
 * in the population has been stabilized.
 */
trait HyperVolumeStabilityTermination extends Termination with ReferencePoint with Dominance with RankMF with StabilityTermination with MG {

  override def terminated(population: => Population[G, P, F, MF], terminationState: STATE): (Boolean, STATE) = {
    val p = population
    val rankMax = p.map { e => rank.get(e.metaFitness)() }.max
    val front = p.filter(e => rank.get(e.metaFitness)() == rankMax).map { i => fitness.get(i.toIndividual.fitness) }
    val hv = Hypervolume(front, referencePoint, this)
    stability(terminationState, hv)
  }
}
