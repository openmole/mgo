package fr.iscpif.mgo.ga.selection.stochastic

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

import fr.iscpif.mgo.ga._
import fr.iscpif.mgo._
import fr.iscpif.mgo.ga.domination._
import java.util.Random
import fr.iscpif.mgo.tools.Random._
import selection.{Ranking, Distance}

//PROBLEME DE TYPE ICI
//TODO : IDEM AVEC INDIVIDUALMG et INDIVIDUAL ... IL NOUS FAUT UN SUPERTYPE ?
trait BinaryTournamentNSGA2[I <: Individual[_, _] with Distance with Ranking] extends Selection[I] {
  //TODO : probleme avec la dominance passé ici, elle sert au calcul initial,
  //et pas a la comparaison, donc peut etre il faudra differencie leur utilisation ?
  //(dominanceType: Dominant)

  /**
   * Renvoie une une liste d'individu pour le mating
   * avec un binary tournament sur le rank et la distance.
   */

  def selection(individuals: IndexedSeq[I])(implicit aprng: Random): I =
    binaryTournament(individuals.random(aprng), individuals.random(aprng))

  def binaryTournament[I <: Individual[_, _] with Distance with Ranking](individual1: I, individual2: I)(implicit aprng: Random): I =
    if (individual1.rank < individual2.rank) return individual1
    else if (individual1.rank > individual2.rank) return individual2
    else if (individual1.distance > individual2.distance) return individual1
    else if (individual2.distance > individual1.distance) return individual2
    else if (aprng.nextDouble < 0.5) individual1 else individual2

}
