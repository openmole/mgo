/*
 * Copyright (C) Guillaume Chérel 04/04/14
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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Random._
import util.Random
import scala.collection.Iterator

/**
 * Select the first individuals of the population sorted by fitness. If all individuals have been used, start back with the first
 */
trait SortedTournamentSelection extends Selection with Tournament {
  override def selection(population: Population[G, P, F, MF])(implicit rng: Random): Iterator[Individual[G, P, F]] = {
    val sortedIndividuals = population.content.sortWith((a, b) => tournament(a, b) == a).map(_.toIndividual)

    var itIndivs = sortedIndividuals.iterator
    Iterator.continually {
      if (!itIndivs.hasNext) itIndivs = sortedIndividuals.iterator
      itIndivs.next()
    }
  }
}
