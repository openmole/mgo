/*
 * Copyright (C) 15/11/12 Romain Reuillon
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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import tools._

import RankDiversityModifier._

trait MapModifier <: Modifier
    with MapPlotter
    with Aggregation
    with RankDiversityModifier
    with HierarchicalRanking
    with NoDiversity {

  override def modify(individuals: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] =
    if (individuals.isEmpty) Population.empty
    else {
      val modified = individuals.map(i => Seq(aggregate(i.fitness)))
      val ranks = rank(modified)
      val distances = diversity(modified, ranks)

      toPopulationElements[G, P, F](individuals, ranks, distances)
    }
}
