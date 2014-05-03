/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package fr.iscpif.mgo.modelfamily

import fr.iscpif.mgo._
import scala.util.Random
import fr.iscpif.mgo.crossover.SBXBoundedCrossOver

trait ModelFamilyCrossover <: CrossOver with ModelFamilyGenome with SBXBoundedCrossover {

  override def crossover(g1: G, g2: G, population: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random) = {
    val (res1, res2) = super.sbxCrossover(g1, g2)

    rng.nextDouble match {
      case x if x < 0.5 => Seq(res1, res2)
      case _ =>
        val m1 = modelId.get(res1)
        val m2 = modelId.get(res2)
        Seq(modelId.set(res1, m2), modelId.set(res2, m1))
    }
  }

}
