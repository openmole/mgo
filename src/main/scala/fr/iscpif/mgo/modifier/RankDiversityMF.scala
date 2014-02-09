/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy
import scalaz._

object RankDiversityMF {
  case class RankDiversity(rank: Lazy[Int], diversity: Lazy[Double]) {
    override def toString = rank + " " + diversity
  }

  def toPopulationElements[G, P, F](
    evaluated: Seq[Individual[G, P, F]],
    ranks: Seq[Lazy[Int]],
    distances: Seq[Lazy[Double]]) =
    (evaluated zip ranks zip distances) map {
      case ((i, r), d) =>
        PopulationElement(
          i,
          RankDiversity(
            diversity = d,
            rank = r
          )
        )
    }
}

trait RankDiversityMF <: MF with DiversityModifier with RankModifier {
  type MF = RankDiversityMF.RankDiversity
  def diversity = Lens.lensu[MF, Lazy[Double]]((c, v) => c.copy(diversity = v), _.diversity)
  def rank = Lens.lensu[MF, Lazy[Int]]((c, v) => c.copy(rank = v), _.rank)
}

