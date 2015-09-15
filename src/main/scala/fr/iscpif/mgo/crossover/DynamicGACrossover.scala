/*
 * Copyright (C) 2015 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import fr.iscpif.mgo.breed.BreedingContextId

trait DynamicGACrossover <: DynamicCrossover with GA with BreedingContextId {

  def crossovers = Vector[Crossover](
    SBXCrossover.apply[G, P, F, A, BreedingContext](0.1)(values),
    BLXCrossover.apply[G, P, F, A, BreedingContext](0.5)(values)
  )

}
