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

trait MapArchive extends Archive with Plotter with Aggregation {
  type A = Map[(Int, Int), Individual[G, F]]

  def initialArchive: A = Map.empty

  def archive(archive: A, individuals: Seq[Individual[G, F]]): A = {
    val tmpArchive = mutable.Map(archive.toSeq: _*)
    for {
      i <- individuals
    } {
      val (x, y) = plot(i.genome)
      tmpArchive.get(x, y) match {
        case Some(ai) => if (aggregate(i.fitness) > aggregate(ai.fitness)) tmpArchive((x, y)) = i
        case None => tmpArchive((x, y)) = i
      }
    }
    tmpArchive.toMap
  }

}
