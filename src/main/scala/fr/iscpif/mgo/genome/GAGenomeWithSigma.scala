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

object GAGenomeWithSigma {

  def apply(_values: Seq[Double], _sigma: Seq[Double]) = new GAGenomeWithSigma {
    val values = _values
    val sigma = _sigma
  }

}

/**
 * Genome for genetic algorithm with an autoadaptative sigma component
 */
trait GAGenomeWithSigma extends GAGenome with Sigma {
  def content = values ++ sigma

  override def updatedValues(values: Seq[Double]) = GAGenomeWithSigma(values, this.sigma).content
  override def updatedSigma(sigma: Seq[Double]) = GAGenomeWithSigma(this.values, sigma).content

}

