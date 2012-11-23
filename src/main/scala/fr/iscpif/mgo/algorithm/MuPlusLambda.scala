/*
 * Copyright (C) 21/11/12 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import java.util.Random

trait MuPlusLambda extends Evolution with Breeding with Elitism {
  override def evolve(population: Population[G, F, MF], archive: A, evaluator: G => F)(implicit aprng: Random): (Population[G, F, MF], A) = {
    val offspring = breed(
      population
    ).par.map { g => Individual(g, evaluator) }.seq

    val newIndividuals = population.toIndividuals ++ offspring
    val newArchive = combine(archive, toArchive(offspring))

    //Elitism strategy
    (elitism(toPopulation(newIndividuals, newArchive)), newArchive)
  }
}
