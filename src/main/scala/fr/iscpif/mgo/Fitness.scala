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

object Fitness {

  implicit def indexedSeqToFit(f: IndexedSeq[Double]) = new {
    def toGAFitness = new Fitness {
      val values = f
    }
  }
  
  def apply(v: Traversable[Double]) = new Fitness {
    val values = v.toIndexedSeq
  }
  
}


trait Fitness {
  def values: IndexedSeq[Double]
  override def toString = values.toString
}
