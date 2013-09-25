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

trait MapModifier extends Modifier with MapPlotter with Aggregation with RankDiversityModifier {

  def neighbours: Int

  override def modify(individuals: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] =
    if (individuals.isEmpty) Population.empty
    else {
      val map = individuals.groupBy(plot)

      val xSize = map.keysIterator.map(_._1).max + 1
      val ySize = map.keysIterator.map(_._2).max + 1

      val matrix =
        NeighborMatrix(
          (x, y) =>
            map.get(x, y) match {
              case Some(e) => if (e.isEmpty) None else Some(e)
              case None => None
            },
          xSize,
          ySize
        )

      def fitness(i: Individual[G, P, F]) = {
        val (x, y) = plot(i)

        val knn = matrix.knn(x, y, neighbours)
        val distance =
          knn.map {
            case (x1, y1) => matrix.distance(x, y, x1, y1)
          }.sum

        val avgFitness =
          (knn.flatMap {
            case (x1, y1) => matrix.matrix(x1, y1).get
          }.map(i => aggregate(i.fitness)).sum) / neighbours

        Seq(aggregate(i.fitness) / avgFitness, 1.0 / distance)
      }

      val modified = individuals.map(fitness)
      val ranks = rank(modified)
      val distances = diversity(modified, ranks)

      toPopulationElements[G, P, F](individuals, ranks, distances)
    }
}
