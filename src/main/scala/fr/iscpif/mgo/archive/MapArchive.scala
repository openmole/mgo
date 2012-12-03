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
  case class MapElement(value: Double, hits: Int) {
    def isEmpty = hits == 0
    def +(o: MapElement) = MapElement.+(this, o)
  }

  object MapElement {
    val empty = MapElement(Double.PositiveInfinity, 0)

    def +(e1: MapElement, e2: MapElement) = MapElement(math.min(e1.value, e2.value), e1.hits + e2.hits)
    def -(e1: MapElement, e2: MapElement) = MapElement(math.min(e1.value, e2.value), e1.hits - e2.hits)
  }

  case class ArchiveMap(values: Array[Array[Double]], hits: Array[Array[Int]], xSize: Int, ySize: Int) {
    def get(x: Int, y: Int) =
      if (x < 0 || y < 0 || x >= xSize || y >= ySize) None else Some(MapElement(values(x)(y), hits(x)(y)))

    def +(o: ArchiveMap) = ArchiveMap.reduce(this, o)(MapElement.+)
    def -(o: ArchiveMap) = ArchiveMap.reduce(this, o)(MapElement.-)

    override def toString = "[" + values.map("[" + _.mkString(", ") + "]").mkString(", ") + "]"
  }

  object ArchiveMap {
    def apply(content: Array[Array[MapElement]], xSize: Int, ySize: Int): ArchiveMap =
      ArchiveMap(content.map(_.map(_.value)), content.map(_.map(_.hits)), xSize, ySize)

    val empty = ArchiveMap(Array.empty, 0, 0)
    def maxSize(a1: ArchiveMap, a2: ArchiveMap) = (math.max(a1.xSize, a2.xSize), math.max(a1.ySize, a2.ySize))
    def reduce(a1: ArchiveMap, a2: ArchiveMap)(op: (MapElement, MapElement) => MapElement) = {
      val (xSize, ySize) = ArchiveMap.maxSize(a1, a2)
      ArchiveMap(
        Array.tabulate(xSize, ySize) {
          case (x, y) =>
            (a1.get(x, y), a2.get(x, y)) match {
              case (Some(e1), Some(e2)) => op(e1, e2)
              case (Some(e1), None) => e1
              case (None, Some(e2)) => e2
              case (None, None) => MapElement.empty
            }
        },
        xSize,
        ySize
      )
    }
  }
}

import MapArchive._

trait MapArchive extends Archive with Plotter with Aggregation {
  type A = ArchiveMap

  def initialArchive: A = ArchiveMap.empty

  def toArchive(individuals: Seq[Individual[G, F]]): A = {
    val sparse = individuals.groupBy(plot).map {
      case (k, v) => k -> v.map(i => MapElement(aggregate(i.fitness), 1)).reduce(_ + _)
    }
    val maxX = sparse.keys.map(_._1).max + 1
    val maxY = sparse.keys.map(_._2).max + 1
    ArchiveMap(
      Array.tabulate[MapElement](maxX, maxY) { case (x, y) => sparse.getOrElse((x, y), MapElement.empty) },
      maxX,
      maxY
    )
  }

  def combine(a1: A, a2: A): A = a1 + a2

  def diff(original: A, modified: A) = modified - original

}
