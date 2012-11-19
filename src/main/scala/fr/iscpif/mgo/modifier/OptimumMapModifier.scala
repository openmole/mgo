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
import tools.NeighborMatrix

import RankDiversityModifier._

trait MapModifier extends Modifier with Plotter with Aggregation with RankModifier with DiversityModifier with MapArchive {

  type F = MGFitness
  type MF = RankDiversity

  def neighbors: Int

  def modify(individuals: IndexedSeq[Individual[G, F]], archive: A): Population[G, F, MF] = {
    val matrix = NeighborMatrix(archive)

    def fitness(i: Individual[G, F]) = {
      val (x, y) = plot(i.genome)
      val distance =
        matrix.knn(x, y, neighbors).map {
          case (x1, y1) => matrix.distance(x, y, x1, y1)
        }.sum
      MGFitness(aggregate(i.fitness), 1.0 / distance)
    }

    val modified = individuals.map(i => Individual(i.genome, fitness(i)))
    toPopulationElements(modified, rank, diversity)
  }
}
