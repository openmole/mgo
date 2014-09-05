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
import tools._
import annotation.tailrec
import util.Random

/**
 * Reduce the size of the population according to a diversity metric and a rank
 */
trait NonDominatedElitism extends Elitism with Mu with Ranking with Diversity {

  override def computeElitism(oldGeneration: Population[G, P, F], offspring: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] = {
    val population = filter(oldGeneration ++ offspring)
    if (population.size < mu) population
    else {
      val ranks = rank(population).map { _() }

      val sortedByRank: List[Seq[PopulationElement[G, P, F]]] =
        (ranks zip population).
          groupBy { case (r, _) => r }.
          toList.
          sortBy { case (r, _) => r }.
          map { case (_, v) => v.map { case (_, e) => e } }

      @tailrec def addFronts(fronts: List[Seq[PopulationElement[G, P, F]]], acc: List[PopulationElement[G, P, F]]): (Seq[PopulationElement[G, P, F]], Seq[PopulationElement[G, P, F]]) = {
        if (fronts.isEmpty) (Seq.empty, acc)
        else if (acc.size + fronts.head.size < mu) addFronts(fronts.tail, fronts.head.toList ::: acc)
        else (fronts.head, acc)
      }

      val (lastFront, selected) = addFronts(sortedByRank, List.empty)

      if (selected.size < mu) {
        selected ++
          (lastFront zip diversity(lastFront)).
          sortBy { case (_, d) => d() }.
          reverse.
          slice(0, mu - selected.size).
          map { case (e, _) => e }
      } else selected
    }
  }
}
