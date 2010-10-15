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



import org.openmole.tools.distrng.prng.IPRNG
import scala.collection.mutable.ArrayBuffer

class EvolutionEngine[T](operations: IndexedSeq[GenomeOperation[T]]) {


    def makeEvolve(genomes: IndexedSeq[T], rng: IPRNG[_]): T = {
        val operation = operations(rng.nextInt(0, operations.size));
        operation.operate(genomes, rng);
    }


    def makeEvolve(genomes: IndexedSeq[T], add: Int, rng: IPRNG[_], selection: Selection[T] = NoSelection) = {
        val ret = new ArrayBuffer[T](add)
        var i = 0

        while (i < add) {
            val operation = operations(rng.nextInt(0, operations.size))

            val newGenome = operation.operate(genomes, rng);

            if(selection.accept(newGenome)) {
                ret += newGenome
                i += 1
            }
        }

        ret.toIndexedSeq
    }

}
