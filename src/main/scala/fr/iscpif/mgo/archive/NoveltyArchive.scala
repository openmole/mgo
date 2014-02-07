/*
 * Copyright (C) 07/02/14 Romain Reuillon
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

trait NoveltyArchive <: Archive with IndividualDistance with ArchiveIndividuals {

  def archiveEpsilon: Double

  def initialArchive = Seq.empty

  def toArchive(individuals: Seq[Individual[G, P, F]]): A = shrink(individuals)

  def combine(a1: A, a2: A): A = shrink(a1 ++ a2)

  def diff(original: A, modified: A): A = modified

  private def shrink(a: A): A = {
    val crowding = individualDistance(a).map(_())
    (a zip crowding).filter { case (_, d) => d > archiveEpsilon }.unzip._1
  }
}
