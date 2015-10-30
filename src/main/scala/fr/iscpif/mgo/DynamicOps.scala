/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.mgo

import scalaz._
import util.Random
import tools._

trait DynamicOps <: Pop {
  def dynamicOperator[OP](genomePart: monocle.Lens[G, Option[Int]], exploration: Double = 0.1)(ops: OP*) = (pop: Pop) => State { rng: Random =>
    def stats(p: Pop) = {
      val working = p.flatMap(i => genomePart get(i.genome))
      val count = working.groupBy(identity)
      (0 until ops.size).map {
        i =>
          val size = count.getOrElse(i, Nil).size
          i -> (size.toDouble / working.size)
      }
    }

    def selected: Int =
      if (rng.nextDouble < exploration) rng.nextInt(ops.size)
      else multinomial(stats(pop).toList)(rng)

    (rng, ops(selected))
  }
}
