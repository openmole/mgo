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

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import genome.G

/**
 * Cake layer to eliminated elements of a population
 */
trait Elitism extends G with F with MF {
  
  /**
   * Reduce the number of elements of the population and return a new one
   * 
   * @param population the population to shrink
   * @return the shrinked population
   */
  def elitism(population: Population[G, F, MF]): Population[G, F, MF]
}
