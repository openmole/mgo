/*
 * Copyright (C) 13/11/12 Romain Reuillon
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

package fr.iscpif.mgo.fitness

object MGFitness {

  implicit def indexedSeqToFit(f: IndexedSeq[Double]) = new {
    def toGAFitness = new MGFitness {
      val values = f
    }
  }

  def apply(v: Traversable[Double]): MGFitness = new MGFitness {
    val values = v.toIndexedSeq.map(v => if (!v.isNaN) v else Double.PositiveInfinity)
  }

  def apply(v: Double*): MGFitness = MGFitness(v)
}

/**
 * The fitness is a vector a of Doubles, one Double for each objective.
 */
trait MGFitness extends Fitness {
  /** The finess value */
  def values: Seq[Double]
  override def toString = values.toString
}