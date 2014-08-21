/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._

import scala.util.Random

trait TournamentOnAggregatedFitness <: Tournament with Aggregation {
  override def tournament(e1: PopulationElement[G, P, F, MF], e2: PopulationElement[G, P, F, MF])(implicit rng: Random): PopulationElement[G, P, F, MF] = {
    val a1 = aggregate(e1.fitness)
    val a2 = aggregate(e2.fitness)
    if (a1 < a2) e1
    else if (a2 < a1) e2
    else if (rng.nextBoolean) e1 else e2
  }
}
