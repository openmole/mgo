/*
 * Copyright (C) 09/02/14 Romain Reuillon
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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.{ PopulationElement, Individual, MF }
import scalaz.Lens

object RankMF {
  case class Rank(rank: Lazy[Int]) {
    override def toString = rank.toString()
  }

  def toPopulationElements[G, P, F](
    evaluated: Seq[Individual[G, P, F]],
    ranks: Seq[Lazy[Int]]) =
    (evaluated zip ranks) map {
      case (i, r) => PopulationElement(i, Rank(rank = r))
    }
}

trait RankMF <: MF with RankModifier {
  type MF = RankMF.Rank
  def rank = Lens.lensu[MF, Lazy[Int]]((c, v) => c.copy(rank = v), _.rank)
}
