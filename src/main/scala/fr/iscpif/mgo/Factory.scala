/*
 * Copyright (C) 2012 reuillon
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

package fr.iscpif.mgo

import java.util.Random

/**
 * A factory of genome
 */
trait Factory[G <: Genome] {
  
  /**
   * Generate a random genome
   */
  def random(implicit rng: Random): G
  
  /**
   * Generate a genome from its internal representation
   * 
   * @param content the internal representation of the genome
   * @retun the genome
   */
  def apply(t: G#T): G
  
  /**
   * Generate an genome from a genome and functions modifing the internal structure
   * of the genome
   * 
   * @param g a genome
   * @param ops functions that modify the internal structure of the genome
   */
  def apply(g: G, ops: G => G#T*): G = 
    ops.foldLeft(g)((g, op) => apply(op(g)))
}
