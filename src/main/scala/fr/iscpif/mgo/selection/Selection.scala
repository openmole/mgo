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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import genome.G
import util.Random
import fr.iscpif.mgo.modifier.MF

/**
 * Selection layer for the evolutionary algorithms.
 */
trait Selection extends G with P with F with MF {

  /**
   * Select an individual among the population.
   *
   * @param population the population in which selection occurs
   * @return the selected individual
   */
  def selection(population: Population[G, P, F, MF])(implicit aprng: Random): Iterator[Individual[G, P, F]]

}
