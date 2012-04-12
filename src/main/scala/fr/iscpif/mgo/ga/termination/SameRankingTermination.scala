package fr.iscpif.mgo.ga.termination

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.{GAFitness, GAGenome}
import fr.iscpif.mgo.ga.domination.Dominant
import fr.iscpif.mgo.ga.selection.{ParetoRank, Rank}
import fr.iscpif.mgo.tools.Math
import fr.iscpif.mgo.ga.selection.Ranking._

/*
 * Copyright (C) 2011 srey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
class SameRankingTermination[I <: Individual[GAGenome, GAFitness]](dominance:Dominant, rank:Rank = new ParetoRank)
  extends AbstractTermination[I]{

  def hasNext(a1: IndexedSeq[I], a2: IndexedSeq[I]): Boolean = {
    Math.allTheSame(
      firstRanked(a1, dominance, rank).map {
        _.fitness.values
      },
      firstRanked(a2, dominance, rank).map {
        _.fitness.values
      }
    )
  }

}
