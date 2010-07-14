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

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

import java.util.LinkedList

class SegmentMutation extends GenomeOperation[GenomeFloat] {

  override def operate(genomes: IGenomes[GenomeFloat], prng: IPRNG[_ <: RNGData]): GenomeFloat = {

    val genome = genomes getRandomGenome(prng)
    val length = prng nextInt(genome.size / 2)
    val tmpValues = new LinkedList[Float]

    genome.foreach( value => {
        tmpValues add (value)
      })

    val firstPos = prng nextInt(tmpValues.size - length)

    var itTmp = tmpValues.listIterator(firstPos)
    val first = new ListBuffer[Float]

    for(i <- 0 until length) {
      first += itTmp.next
      itTmp.remove
    }

    val secondPos = prng nextInt (tmpValues.size - length)
    itTmp = tmpValues listIterator (secondPos)
       
    val second = new LinkedList[Float]

    for(i <- 0 until length) {
      second add (itTmp.next)
      itTmp.remove
    }

    first.foreach( f => {
        itTmp add (f)
      })

    itTmp = tmpValues listIterator(firstPos)

    second.foreach( f => {
        itTmp add(f)
      })

    val ret = new GenomeFloat(new Array[Float](tmpValues.size()));
    var i = 0;

    tmpValues.foreach(f => {
        ret(i) = f
        i+=1
      })

    ret
  }

}
