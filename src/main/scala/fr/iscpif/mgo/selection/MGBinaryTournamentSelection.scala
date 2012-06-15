/*
 * Copyright (C) 2010 reuillon
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
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.dominance._
import java.util.Random
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools.Random._

trait MGBinaryTournamentSelection extends Selection { 
  self: GAEvolution with MG {type I <: Individual[_] with Diversity with Rank} =>

  def selection(individuals: IndexedSeq[I])(implicit aprng: Random): I =
    binaryTournament(individuals.random(aprng), individuals.random(aprng))

  def binaryTournament(individual1: I, individual2: I)(implicit aprng: Random): I =
    if (individual1.rank < individual2.rank) return individual1
    else if (individual1.rank > individual2.rank) return individual2
    else if (individual1.diversity > individual2.diversity) return individual1
    else if (individual2.diversity > individual1.diversity) return individual2
    else if (aprng.nextDouble < 0.5) individual1 else individual2

}
