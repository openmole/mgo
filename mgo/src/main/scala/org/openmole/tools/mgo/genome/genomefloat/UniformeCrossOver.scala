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
import org.openmole.tools.distrng.data.RNGData
import org.openmole.tools.mgo.model.IGenomes


class UniformeCrossOver extends GenomeOperation[GenomeFloat] {

  override def operate(genomes: IGenomes[GenomeFloat], prng: IPRNG[_ <: RNGData]): GenomeFloat = {
    val genome1 = genomes getRandomGenome(prng)
    val genome2 = genomes getRandomGenome(prng)

    val newGenome = new GenomeFloat(new Array[Float](genome1.size))

    for(i <- 0 until genome1.size) {
      newGenome(i) = if(prng.nextFloat < 0.5) genome1(i) else genome2(i)
    }

    newGenome
  }

}
