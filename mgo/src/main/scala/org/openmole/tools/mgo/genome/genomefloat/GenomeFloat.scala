/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package org.openmole.tools.mgo.genome.genomefloat

import org.openmole.tools.distrng.prng.IPRNG
import org.openmole.tools.distrng.data.RNGData

import java.util.ArrayList
import scala.Array
import scala.reflect.Manifest

class GenomeFloat(val chromosomes: Array[Float])  {
  def apply(i: Int): Float = {chromosomes.apply(i)}
  def update(i: Int, v: Float) = {chromosomes.update(i, v)}
  def size(): Int = {chromosomes.size}
  def foreach(f: Float => Unit) = {chromosomes.foreach(f)}
}
