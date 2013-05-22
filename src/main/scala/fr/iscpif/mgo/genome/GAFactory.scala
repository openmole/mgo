/*
 * Copyright (C) 20/11/12 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.genome

import fr.iscpif.mgo._
import util.Random

trait GAFactory extends GenomeFactory with GA {

  /** Size of the value part of the genome */
  def genomeSize: Int

  def genomeFactory: Factory[G] = new Factory[G] {
    def apply(content: GAGenome#T) = {
      assert(content.size == genomeSize)
      GAGenome(content)
    }

    def random(implicit rng: Random) = apply(Stream.continually(rng.nextDouble).take(genomeSize).toIndexedSeq)
  }

}
