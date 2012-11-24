/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import genome.G

/**
 * The modifier layer decorates individuals with a meta-fitness which is function of
 * the population.
 */
trait Modifier extends G with F with MF with IndividualFilter with A {
  /**
   * Generate a population from a set of indiviuals that is filtered in a first time
   *
   * @param individuals a set of individual
   * @return the filtred population with the meta-fitness for each individual
   */
  def toPopulation(individuals: Seq[Individual[G, F]], a: A): Population[G, F, MF] = modify(filter(individuals), a)

  /**
   * Generate a population from a set of indiviuals
   *
   * @param individuals a set of individual
   * @return the population with the meta-fitness for each individual
   */
  def modify(individuals: Seq[Individual[G, F]], archive: A): Population[G, F, MF]

}
