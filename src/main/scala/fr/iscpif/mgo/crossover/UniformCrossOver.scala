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
import util.Random
import fr.iscpif.mgo.tools.Random._

/**
 * Swap part of each genome
 */
trait UniformCrossOver extends CrossOver with GenomeFactory {

  type G <: genome.GAGenome

  /** Average rate of exchange between the 2 genomes */
  def crossoverRate: Double = 0.5

  def crossover(g1: G, g2: G)(implicit aprng: Random) = {
    val rngValue = (0 until g1.content.size).map { x => !(aprng.nextDouble < crossoverRate) }
    val offspringValues = (rngValue zip (g1.content zip g2.content)) map {
      case (b, (g1e, g2e)) =>
        if (b) (g1e, g2e) else (g2e, g1e)
    }

    IndexedSeq(genomeFactory(offspringValues.map { _._1 }), genomeFactory(offspringValues.map { _._2 }))
  }

}

