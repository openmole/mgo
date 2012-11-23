/*
 * Copyright (C) 19/11/12 Romain Reuillon
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

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._
import collection.mutable

object MapArchive {
  case class MapElement(value: Double, hits: Int = 1) {
    def combine(o: MapElement) =
      if (o.value < value) copy(value = o.value, hits = hits + o.hits)
      else copy(hits = hits + o.hits)
  }
}

import MapArchive._

trait MapArchive extends Archive with Plotter with Aggregation {
  type A = Map[(Int, Int), MapElement]

  def initialArchive: A = Map.empty

  def toArchive(individuals: Seq[Individual[G, F]]): A = {
    val tmpArchive = mutable.Map.empty[(Int, Int), MapElement]
    for (i <- individuals) {
      val (x, y) = plot(i)
      val value = aggregate(i.fitness)
      tmpArchive.get(x, y) match {
        case Some(e) => tmpArchive((x, y)) = e.combine(MapElement(value))
        case None => tmpArchive((x, y)) = MapElement(value)
      }
    }
    tmpArchive.toMap
  }

  def combine(a1: A, a2: A): A = {
    val tmpArchive = mutable.Map.empty[(Int, Int), MapElement]
    for (((x, y), me) <- a1.toSeq ++ a2.toSeq)
      tmpArchive.get(x, y) match {
        case Some(e) => tmpArchive((x, y)) = e.combine(me)
        case None => tmpArchive((x, y)) = me
      }
    tmpArchive.toMap
  }

}
