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

/**
 * Auto-adaptative sigma values for the mutation of genetic algorithms
 */
trait Sigma { self: GAGenome =>
  /** Sigma values, one for each genome component */
  def sigma: IndexedSeq[Double]
  
  /**
   * Update the sigma part of the genome and retun the new internal structure
   * 
   * @param sigma the sigma part of the genome
   * @return the updated internal representation
   */
  def updatedSigma(sigma: IndexedSeq [Double]): IndexedSeq[Double]
}
