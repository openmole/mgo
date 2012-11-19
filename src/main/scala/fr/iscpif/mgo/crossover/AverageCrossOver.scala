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

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.GenomeFactory
import java.util.Random

/**
 * Compute a ponderated average between 2 genomes
 */
trait AverageCrossover extends CrossOver with GenomeFactory {

  type G <: genome.GAGenome

  def crossover(g1: G, g2: G)(implicit aprng: Random) = {
    val pds = aprng.nextDouble

    val newValues = IndexedSeq.tabulate(g1.values.size)(i =>
      (pds * g1.values(i) + (1 - pds) * g2.values(i)) / 2)

    IndexedSeq(genomeFactory(newValues))
  }
}

