/*
 * Copyright (C) 2011 Sebastien Rey
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

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import annotation.tailrec

/**
 * Reduce the size of the population according to a divesity metric and a rank
 */
trait NonDominatedElitism extends Elitism with Mu {
  type MF <: Diversity with Rank

  def elitism(individuals: Seq[Individual[G, P, F]], archive: A): Seq[Individual[G, P, F]] = {
    if (individuals.size < mu) individuals
    else {
      val population = toPopulation(individuals, archive)
      val fronts = population.groupBy(_.metaFitness.rank()).toList.sortBy(_._1).map { case (_, e) => e.map(i => i.individual -> i.metaFitness.diversity) }

      type FE = (Individual[G, P, F], Lazy[Double])

      @tailrec def addFronts[I](fronts: List[Seq[FE]], acc: List[Seq[FE]], size: Int = 0): (Seq[FE], Seq[FE]) = {
        if (size + fronts.head.size < mu) addFronts[I](fronts.tail, fronts.head :: acc, size + fronts.head.size)
        else (fronts.headOption.getOrElse(Seq.empty), acc.flatten)
      }

      val (lastFront, selected) = addFronts(fronts, List.empty)

      (if (selected.size < mu)
        selected ++ lastFront.sortBy(_._2()).reverse.slice(0, mu - selected.size)
      else selected).map(_._1)
    }
  }
}
