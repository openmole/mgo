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

import scala.util.Random

/*trait NoveltyArchive <: Archive with IndividualDistanceFromArchive with ArchiveIndividuals {

  def archiveEpsilon: Double

  def initialArchive(implicit rng: Random) = Seq.empty

  def archive(a1: A, oldIndividuals: Population[G, P, F], offspring: Population[G, P, F])(implicit rng: Random) = offspring.toIndividuals.foldLeft(a1)((a, i) => addMaybe(a, i))

  def addMaybe(a: A, i: Individual[G, P, F]): A = {
    if (distanceOfIndividualFromArchive(i, a)() > archiveEpsilon) a :+ i
    else a
  }
}
*/ 