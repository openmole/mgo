/*
 * Copyright (C) 16/11/13 Romain Reuillon
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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import scala.util.Random
import fr.iscpif.mgo.tools.NeighborMatrix

trait MapSelection <: Selection
    with MapPlotter
    with Aggregation {

  def neighbourPressure: Int = 8
  def tournamentSize: Int = 1

  def selection(population: Population[G, P, F], a: A)(implicit rng: Random) = {
    assert(!population.isEmpty)
    val matrix = NeighborMatrix(population.toIndividuals, plot _)

    val coodinates =
      Iterator.continually { (rng.nextInt(matrix.maxX), rng.nextInt(matrix.maxY)) }

    Iterator.continually {
      def fitnesses =
        for {
          (x, y) <- coodinates.take(tournamentSize)
          (ix, iy) <- matrix.knn(x, y, neighbourPressure) ++ Seq(x -> y)
          i <- matrix.matrix(ix, iy)
        } yield i -> aggregate(i.fitness)

      fitnesses.minBy(_._2)._1
    }
  }

}
