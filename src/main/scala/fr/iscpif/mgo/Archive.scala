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

package fr.iscpif.mgo

import scalaz._

trait Archive <: Pop { this: Algorithm =>
  trait Archive <: State[AlgorithmState, Unit]
}

trait ArchiveFunctions <: Archive with Niche { this: Algorithm =>

  def hitMap[A](offspring: Pop)(implicit stateLens: monocle.Lens[AlgorithmState, STATE], archive: monocle.Lens[STATE, scala.collection.Map[A, Int]], niche: Niche[A]) = new Archive {

    override def apply(state: AlgorithmState) = {
      (stateLens composeLens archive).modify { archive =>
          val offSpringArchive = offspring.groupBy(niche).map { case (k, v) => (k -> v.size) }

        offSpringArchive.foldLeft(archive) {
          case (a, (a2key, a2value)) =>
            if (a contains a2key) a + ((a2key, a(a2key) + a2value))
            else a + ((a2key, a2value))
        }
      }(state)
    }
  }

}

