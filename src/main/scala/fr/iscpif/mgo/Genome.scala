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

import monocle._

import scala.util.Random
import scalaz.State

object Genome {
  case class GenomeValue[V](val value: V) extends AnyVal
}

import Genome._

trait Genome { this: Algorithm =>

  implicit def genomeValueToV[V](v: GenomeValue[V]) = v.value
  implicit class GenomeDecorator[G](g: G) {
    def genomeValue[V](implicit lens: monocle.Lens[G, GenomeValue[V]]) = lens.get(g).value
  }

  type RandomGenome = State[Random, G]

  def clamp(g: G)(implicit values: Lens[G, GenomeValue[Seq[Double]]]) =
    values.modify { v => GenomeValue(v.value.map(tools.Math.clamp(_, 0.0, 1.0))) }(g)

}
