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

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._

trait Archive extends G with P with F with MF with A {
  type A = Seq[ArchiveElement]
  type ArchiveElement

  def initialArchive: A

  def archive(a: A, individuals: Seq[Individual[G, P, F]]) =
    combine(a, toArchive(individuals))

  def toArchive(individuals: Seq[Individual[G, P, F]]): A
  def combine(a1: A, a2: A): A
  def diff(original: A, modified: A): A

}
