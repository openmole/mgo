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

package fr.iscpif.mgo.ga

import fr.iscpif.mgo._

/**
 * Genome for genetic algorithm with an autoadaptative sigma component
 */
case class GAGenomeWithSigma(
  val values: IndexedSeq[Double],
  val sigma: IndexedSeq[Double]) extends GAGenome with Sigma {
  
  def content = values ++ sigma
  
  override def updatedValues(values: IndexedSeq [Double]) = copy(values = values).content

  override def updatedSigma(sigma: IndexedSeq [Double]) = copy(sigma = sigma).content
                                  
}



