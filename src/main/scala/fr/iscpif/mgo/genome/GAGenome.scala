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

package fr.iscpif.mgo.genome

import fr.iscpif.mgo._

object GAGenome {
  def apply(v: Seq[Double]) = new GAGenome {
    val values = v
    val content = v
    def updatedValues(values: Seq[Double]) = values
  }
}

/**
 * Genome for genetic algorithms
 */
trait GAGenome extends Genome {
  type T = Seq[Double]

  /**
   * The sequence of values representing the candidate solution. Values evolve
   * in the interval [0.0, 1.0]. They are scaled when they are provided to the
   * fitness function
   */
  def values: T

  /**
   * Update the value part of the genome
   *
   * @param values the new values
   * @return the new internal representation of the genome
   */
  def updatedValues(values: Seq[Double]): T

  override def toString = content.toString
}
