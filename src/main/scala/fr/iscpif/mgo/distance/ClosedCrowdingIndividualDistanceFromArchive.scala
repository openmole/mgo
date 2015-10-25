/*
 * Copyright (C) 03/03/14 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.distance

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.tools.metric.ClosedCrowdingDistance

import scala.util.Random

//trait ClosedCrowdingIndividualDistanceFromArchive <: IndividualDistanceFromArchive with IndividualPosition with ArchiveIndividuals {
//
//  def distanceOfIndividualFromArchive(i: Individual[G, P, F], a: A)(implicit rng: Random): Lazy[Double] =
//    ClosedCrowdingDistance.of(0, individualPosition(i) +: a.map(individualPosition))
//
//}
