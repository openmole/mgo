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

import scala.util.Random
import scalaz._

object genome {
  sealed trait Value
  sealed trait From

  implicit class GenomeDecorator[G](g: G) {
    def genomeValue[V](implicit lens: monocle.Lens[G, V @@ Value]): V = Tag.unwrap(lens.get(g))
  }

  implicit class IndividualGenomeDecorator[G, P](i: Individual[G, P]) {
    def genomeValue[V](implicit lens: monocle.Lens[G, V @@ Value]): V = Tag.unwrap(lens.get(i.genome))
  }

  type RandomGenome[G] = State[Random, G]

  def clamp[G](g: G)(implicit values: monocle.Lens[G, Seq[Double] @@ Value]) =
    values.modify { v => v.map(tools.Math.clamp(_, 0.0, 1.0)) }(g)

}
