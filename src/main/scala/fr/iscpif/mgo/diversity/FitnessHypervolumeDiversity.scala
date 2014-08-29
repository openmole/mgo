/*
 * Copyright (C) 2011 sebastien rey
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

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo.tools.metric.Hypervolume.ReferencePoint
import fr.iscpif.mgo.tools.metric.Hypervolume

import scala.math._

/**
 * Diversity computed from an hypervolume contribution metric
 *
 * @see Hypervolume
 */
trait FitnessHypervolumeDiversity extends Diversity with ReferencePoint with MG {

  override def diversity(values: Population[G, P, F]) =
    Hypervolume.contributions(values.map(e => fitness(e.toIndividual)), referencePoint)

}
