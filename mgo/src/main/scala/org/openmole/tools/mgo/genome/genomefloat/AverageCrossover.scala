/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
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

import java.util.Random
import org.openmole.tools.mgo.evolution.GenomeOperation
import GenomeFloat._

class AverageCrossover extends GenomeOperation[GenomeFloat] {
  
  override def operate(genomes: IndexedSeq[GenomeFloat])(implicit prng: Random): GenomeFloat = {
    val size = genomes.head.size
    
    val genome1 = randomGenome(size)
    val genome2 = randomGenome(size)
  
    val newGenome = new GenomeFloat(new Array[Float](genome1.size))

    for(i <- 0 until size) {
      val coef = prng.nextFloat
      newGenome(i) = (genome1(i) * coef + genome2(i) * (1 - coef)) / 2
    }
    
    newGenome
  }

}
