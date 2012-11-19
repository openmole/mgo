/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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
import tools.Lazy

object RankDiversityModifier {

  def toPopulationElements[G, F](
    evaluated: Seq[Individual[G, F]],
    rank: Seq[Individual[G, F]] => Seq[Lazy[Int]],
    diversity: (Seq[Individual[G, F]], Seq[Lazy[Int]]) => Seq[Lazy[Double]]) = {
    val ranks = rank(evaluated)
    val distances = diversity(evaluated, ranks)
    (evaluated zip ranks zip distances) map {
      case ((i, r), d) =>
        PopulationElement(
          i,
          new RankDiversity(
            diversity = d,
            rank = r
          )
        )
    }
  }

}

import RankDiversityModifier._

/**
 * Compute a meta-fitness with a rank an a diversity
 */
trait RankDiversityModifier extends RankModifier with DiversityModifier {

  override type MF = RankDiversity

  override def modify(evaluated: Seq[Individual[G, F]], archive: A) =
    toPopulationElements[G, F](evaluated, rank, diversity)
}
