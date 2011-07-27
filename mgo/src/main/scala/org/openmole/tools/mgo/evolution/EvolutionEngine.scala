/*
 * Copyright (C) 2010 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package org.openmole.tools.mgo.evolution

import scala.collection.mutable.ArrayBuffer
import org.openmole.tools.mgo._
import org.openmole.tools.mgo.model._

import java.util.Random

class EvolutionEngine[G <: AbstractGenome, F <: GenomeFactory[G]](operations: Operator [G,F]*) {
  
  def apply(genomes: IndexedSeq[G])(implicit aprng: Random): G = {
    val operation = operations(aprng.nextInt(operations.size))
    operation.operate(genomes)
  }

  def apply(genomes: IndexedSeq[G], add: Int, selection: Selection[G] = NoSelection)(implicit aprng: Random) = {
    val ret = new ArrayBuffer[G](add)
    var i = 0

    while (i < add) {
      val operation = operations(aprng.nextInt(operations.size))

      val newGenome = operation.operate(genomes)

      if(selection.accept(newGenome)) {
        ret += newGenome
        i += 1
      }
    }
    
    ret.toIndexedSeq
  }

}
