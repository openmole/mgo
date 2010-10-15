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

import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.distrng.prng.IPRNG
import GenomeFloat._


class RandomMutation extends GenomeOperation[GenomeFloat] {

  override def operate(genomes: IndexedSeq[GenomeFloat], prng: IPRNG[_]): GenomeFloat = {
    val size = genomes.head.size
    val mutationRate = prng.nextFloat
    val genome = randomGenome(size, prng)

    val newGenome = new GenomeFloat(new Array[Float](size))
    var i = 0

    genome.foreach( e => {
        newGenome(i) = {if(prng.nextFloat < mutationRate) e else prng.nextFloat}
        i+=1
      })
    
    newGenome
  }

}
