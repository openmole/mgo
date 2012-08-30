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
import java.util.Random
import fr.iscpif.mgo.tools.Random._

/**
 * Select the best ranked and if equal the more diverse individual between
 * two individual randomly drawn in the population.
 */
trait BinaryTournamentSelection extends Selection { 
  self: G with MF {type MF <: Diversity with Rank} =>

  def selection(population: Population[G, MF])(implicit aprng: Random): Individual[G] =
    binaryTournament(population.content.random(aprng), population.content.random(aprng)).toIndividual

  /**
   * Select the best ranked and if equal the more diverse individual between
   * two population elements.
   * 
   * @param e1 the first population element
   * @param e2 the second population element
   * @return the winning population element
   */
  def binaryTournament(e1: PopulationElement[G, MF], e2: PopulationElement[G, MF])(implicit aprng: Random): PopulationElement[G, MF] =
    if (e1.metaFitness.rank() < e2.metaFitness.rank()) e1
    else if (e1.metaFitness.rank() > e2.metaFitness.rank()) e2
    else if (e1.metaFitness.diversity() > e2.metaFitness.diversity()) e1
    else if (e2.metaFitness.diversity() > e1.metaFitness.diversity()) e2
    else if (aprng.nextDouble < 0.5) e1 else e2

}
