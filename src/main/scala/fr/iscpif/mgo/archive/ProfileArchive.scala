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

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._

object ProfileArchive {
  case class ArchiveProfile(values: Array[Double], hits: Array[Int], size: Int) {
    def get(x: Int) =
      if (x < 0 || x >= size) None else Some(PlotElement(values(x), hits(x)))

    def +(o: ArchiveProfile) = ArchiveProfile.reduce(this, o)(PlotElement.+)
    def -(o: ArchiveProfile) = ArchiveProfile.reduce(this, o)(PlotElement.-)

    override def toString = "[" + values.mkString(", ") + "]"
  }

  object ArchiveProfile {
    def apply(content: Array[PlotElement], size: Int): ArchiveProfile =
      ArchiveProfile(content.map(_.value), content.map(_.hits), size)

    val empty = ArchiveProfile(Array.empty, 0)
    def maxSize(a1: ArchiveProfile, a2: ArchiveProfile) = math.max(a1.size, a2.size)
    def reduce(a1: ArchiveProfile, a2: ArchiveProfile)(op: (PlotElement, PlotElement) => PlotElement) = {
      val size = ArchiveProfile.maxSize(a1, a2)
      ArchiveProfile(
        Array.tabulate(size) {
          x =>
            (a1.get(x), a2.get(x)) match {
              case (Some(e1), Some(e2)) => op(e1, e2)
              case (Some(e1), None) => e1
              case (None, Some(e2)) => e2
              case (None, None) => PlotElement.empty
            }
        },
        size
      )
    }
  }

}

import ProfileArchive._

trait ProfileArchive extends Archive with ProfilePlotter with Aggregation {
  type A = ArchiveProfile

  def initialArchive: A = ArchiveProfile.empty

  def toArchive(individuals: Seq[Individual[G, P, F]]): A = {
    val indexed: Map[Int, PlotElement] = individuals.groupBy(plot).map {
      case (k, v) => k -> v.map(i => PlotElement(aggregate(i.fitness), 1)).reduce(_ + _)
    }
    val size = indexed.map(_._1).max + 1
    ArchiveProfile(Array.tabulate(size)(i => indexed.getOrElse(i, PlotElement.empty)), size)
  }

  def combine(a1: A, a2: A): A = a1 + a2

  def diff(original: A, modified: A) = modified - original
}
