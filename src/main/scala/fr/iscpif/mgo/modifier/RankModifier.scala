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
import fr.iscpif.mgo.tools.Lazy
import monocle.SimpleLens
import monocle.Macro._

/**
 * Layer for modifier that adds a rank to the meta-fitness
 */
object RankModifier {
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

trait RankModifier extends Modifier with Ranking with RankMF with MG with ModifiedFitness {
  type MF = RankModifier.Rank

  def rank = mkLens("rank")

  override def modify(evaluated: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] = {
    val ranks = rank(fitnesses(evaluated, archive))
    RankModifier.toPopulationElements[G, P, F](evaluated, ranks)
  }
}

