package fr.iscpif.mgo.ga.termination

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.{GAFitness, GAGenome}
import fr.iscpif.mgo.ga.domination.Dominant
import fr.iscpif.mgo.ga.selection.{ParetoRank, Rank}
import fr.iscpif.mgo.tools.Math
import fr.iscpif.mgo.ga.selection.Ranking._
import fr.iscpif.mgo.ga.algorithm.{MOOElitism, Evolution}
import fr.iscpif.mgo.ga.selection._

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
trait SameRankingTermination[I <: Individual[GAGenome, GAFitness] with Ranking]
  extends AbstractTermination[I] with CounterTermination {

  def maxSameValue:Int

  private var counter:Int = 0

  def terminated(a1: IndexedSeq[I], a2: IndexedSeq[I], step: Int): Boolean = {

    if ( Math.allTheSame(firstRanked(a1).map {_.fitness.values},firstRanked(a2).map {_.fitness.values}))
      counter += 1
    else
      counter = 0

    println ("generation : " + step)

    if (counter >= maxSameValue || step >= maxStep) return true
    else return false

    }

}
