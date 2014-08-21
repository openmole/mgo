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
import util.Random

/**
 * Swap part of each genome
 */
trait UniformCrossover extends Crossover with GA {

  /** Average rate of exchange between the 2 genomes */
  def crossoverRate: Double = 0.5

  override def crossover(g1: G, g2: G, population: Seq[Individual[G, P, F]], archive: A)(implicit aprng: Random) = {
    val rngValue = (0 until fullGenome.get(g1).size).map { x => !(aprng.nextDouble < crossoverRate) }
    val offspringValues = (rngValue zip (fullGenome.get(g1) zip fullGenome.get(g2))) map {
      case (b, (g1e, g2e)) =>
        if (b) (g1e, g2e) else (g2e, g1e)
    }

    IndexedSeq(fullGenome.set(g1, offspringValues.map { _._1 }), fullGenome.set(g2, offspringValues.map { _._2 }))
  }

}

