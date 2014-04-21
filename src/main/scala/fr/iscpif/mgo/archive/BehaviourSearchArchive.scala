
/*
 * Copyright (C) 03/04/14 Guillaume Chérel
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
import collection.mutable.HashMap

trait BehaviourSearchArchive <: Archive with ArchiveMap {

  //  type ArchiveKey = Seq[Int]

  type A = HashMap[ArchiveKey, Individual[G, P, F]]
  type P <: Seq[Double]
  type ArchiveKey = Seq[Int]

  def behaviourSpaceBreaks: Seq[Seq[Double]]

  def initialArchive = HashMap.empty[ArchiveKey, Individual[G, P, F]]

  def toArchive(individuals: Seq[Individual[G, P, F]]): A = individuals.foldLeft(initialArchive)((a, i) => addMaybe(a, i))

  def combine(a1: A, a2: A): A = a2.foldLeft(a1)((a, kv) => addMaybe(a, kv._2))

  def diff(original: A, modified: A): A = modified

  def addMaybe(a: A, i: Individual[G, P, F]): A = {
    val targetCell: ArchiveKey = behaviour2Cell(i.phenotype)
    individualOfCell(targetCell) match {
      case None => a += (targetCell -> i)
      case Some(j) => a += (targetCell -> competeForCell(targetCell, i, j))
    }
  }

  def behaviour2Cell(p: P): ArchiveKey = {
    for (pi <- 0 until (p.size - 1)) yield {
      var cellVal: Int = 0
      while (p(pi) >= p(pi + 1)) { cellVal += 1 }
      cellVal
    }
  }

  def individualOfCell(c: ArchiveKey): Option[Individual[G, P, F]] = ???

  def competeForCell(c: ArchiveKey, i: Individual[G, P, F], j: Individual[G, P, F]): Individual[G, P, F] = ???

}
