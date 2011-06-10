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

import java.util.Random
import org.openmole.tools.mgo.evolution.GenomeOperation
import GenomeFloat._
import org.openmole.tools.mgo.tools.Random._

class RandomMutation(rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[GenomeFloat] {

  def this(rate: Double) = this(_ => rate)
  
  override def operate(genomes: IndexedSeq[GenomeFloat])(implicit prng: Random): GenomeFloat = {
    val size = genomes.head.size
    val mutationRate = rate(prng)
    val genome = genomes.random

    val newGenome = new GenomeFloat(new Array[Float](size))
    var i = 0

    genome.foreach( e => {
        newGenome(i) = {
          if(prng.nextFloat < mutationRate) e else prng.nextFloat}
        i+=1
      })
    
    newGenome
  }

}
