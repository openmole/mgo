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

trait MGBinaryTournamentSelection extends Selection { 
  self: GAEvolution with MG {type MF <: Diversity with Rank} =>

  def selection(population: Population[G, MF])(implicit aprng: Random): Individual[G] =
    binaryTournament(population.content.random(aprng), population.content.random(aprng)).toIndividual

  def binaryTournament(individual1: PopulationElement[G, MF], individual2: PopulationElement[G, MF])(implicit aprng: Random): PopulationElement[G, MF] =
    if (individual1.metaFitness.rank() < individual2.metaFitness.rank()) individual1
    else if (individual1.metaFitness.rank() > individual2.metaFitness.rank()) individual2
    else if (individual1.metaFitness.diversity() > individual2.metaFitness.diversity()) individual1
    else if (individual2.metaFitness.diversity() > individual1.metaFitness.diversity()) individual2
    else if (aprng.nextDouble < 0.5) individual1 else individual2

}
