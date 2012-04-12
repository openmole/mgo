package fr.iscpif.mgo.ga.selection.paretorankingstrategy

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.{GAFitness, GAGenome}
import fr.iscpif.mgo.ga.domination.Dominant
import fr.iscpif.mgo.ga.selection.{Ranking, Distance, ParetoRank, Rank}
import annotation.tailrec

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

//Utiliser par NSGA2 et MO-CMA-ES
class NonDominatedSorting(archiveSize : Int) {

  def apply[G <: GAGenome]( individuals: IndexedSeq[Individual[G, GAFitness] with Distance with Ranking])(implicit dominance: Dominant): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {

    if (individuals.size < archiveSize) individuals
    else {
      val fronts = individuals.groupBy(_.rank).toList.sortBy(_._1).map {
        _._2
      }

      @tailrec def addFronts[I](fronts: List[IndexedSeq[I]], acc: List[I]): (IndexedSeq[I], List[I]) = {
        if (acc.size + fronts.head.size < archiveSize) addFronts(fronts.tail, acc ++ fronts.head)
        else (fronts.headOption.getOrElse(IndexedSeq.empty), acc)
      }

      val (lastFront, selected) = addFronts(fronts, List.empty)

      (if (selected.size < archiveSize) selected ++ lastFront.sortBy(_.distance).reverse.slice(0, archiveSize - selected.size) else selected).toIndexedSeq
    }
  }
}
