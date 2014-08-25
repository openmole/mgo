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
import fr.iscpif.mgo.tools._
import util.Random

/**
 * Select the best ranked and if equal the more diverse individual between
 * two individual randomly drawn in the population.
 */
trait BinaryTournamentSelection extends Selection with OneByOne with Tournament with NumberOfRound {

  def rounds(populationSize: Int) = 1

  override def selectOne(population: Population[G, P, F, MF])(implicit rng: Random): Individual[G, P, F] = {
    def newChallenger: PopulationElement[G, P, F, MF] = population.content.random

    def round(champion: PopulationElement[G, P, F, MF], rounds: Int): PopulationElement[G, P, F, MF] =
      if (rounds <= 0) champion
      else round(tournament(champion, newChallenger), rounds - 1)

    round(newChallenger, rounds(population.size)).toIndividual
  }

}
