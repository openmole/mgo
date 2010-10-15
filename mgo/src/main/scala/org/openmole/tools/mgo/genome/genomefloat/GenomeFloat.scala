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
import org.openmole.tools.mgo.evolution.Selection
import org.openmole.tools.mgo.evolution.NoSelection

import scala.Array


class GenomeFloat(val chromosomes: Array[Float]) {
  def apply(i: Int): Float = {chromosomes.apply(i)}
  def update(i: Int, v: Float) = {chromosomes.update(i, v)}
  def size(): Int = {chromosomes.size}
  def foreach(f: Float => Unit) = {chromosomes.foreach(f)}
}

object GenomeFloat {

  def randomGenome(length: Int, rng: IPRNG[_]): GenomeFloat = {
    val genome = new Array[Float](length)

    for(i <- 0 until length) {
      genome(i) = rng.nextFloat;
    }

    new GenomeFloat(genome)
  }

 
  def randomGenomes(length: Int, rng: IPRNG[_], number: Int, selection: Selection[GenomeFloat] = NoSelection): IndexedSeq[GenomeFloat] = {
    val genomes = new Array[GenomeFloat](number)

    for(i <- 0 until number) {
      var genome: GenomeFloat = null
      do {
        genome = randomGenome(length, rng)
      } while(!selection.accept(genome))
      genomes(i) = genome
    }

    genomes
  }
}
