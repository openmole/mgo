/*
 * Copyright (C) 07/01/13 Romain Reuillon
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

import RankDiversityModifier._

trait ProfileModifier extends Modifier with ProfilePlotter with Aggregation with RankDiversityModifier {

  type A <: ProfileArchive#A

  override def modify(individuals: Seq[Individual[G, F]], archive: A): Population[G, F, MF] = {
    def findNeighbors(place: Int, range: Int = 1): Int = {
      val minBound = math.max(place - range, 0)
      val maxBound = math.min(place + range, archive.size - 1)

      val bounded = (minBound to maxBound).flatMap(archive.get)
      if (!bounded.isEmpty || (minBound <= 0 && maxBound >= archive.size - 1)) range
      else findNeighbors(place, range + 1)
    }

    def fitness(i: Individual[G, F]) = {
      val x = plot(i)
      val distance = findNeighbors(x)

      val hitCount: Double =
        archive.get(x).map(_.hits) match {
          case Some(v) => v
          case None => 0
        }
      MGFitness(aggregate(i.fitness), 1.0 / distance, hitCount)
    }

    val modified = individuals.map(fitness)
    val ranks = rank(modified)
    val distances = diversity(modified, ranks)

    toPopulationElements[G, F](individuals, ranks, distances)
  }
}

