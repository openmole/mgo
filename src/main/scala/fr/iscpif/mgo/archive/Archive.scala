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

import fr.iscpif.mgo
import fr.iscpif.mgo._
import fr.iscpif.mgo.elitism.Niche
import scalaz._

trait Archive <: Pop { this: Algorithm =>
  trait Archive <: State[EvolutionState, Unit]
}

trait ArchiveDefault <: Archive with Niche { this: Algorithm =>

  def hitMap[A](offspring: Pop)(implicit archive: monocle.Lens[STATE, Map[A, Int]], niche: Niche[A]) = new Archive {

    override def apply(state: EvolutionState) = {
      (EvolutionState.state composeLens archive).modify { archive =>
          val offSpringArchive = offspring.content.groupBy(niche).map { case (k, v) => (k -> v.size) }

        offSpringArchive.foldLeft(archive) {
          case (a, (a2key, a2value)) =>
            if (a contains a2key) a + ((a2key, a(a2key) + a2value))
            else a + ((a2key, a2value))
        }
      }(state)
    }
  }

}
