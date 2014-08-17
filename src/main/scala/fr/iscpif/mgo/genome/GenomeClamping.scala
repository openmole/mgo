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

package fr.iscpif.mgo.genome

import monocle.SimpleLens
import fr.iscpif.mgo._

trait GenomeClamping <: G {
  def clamp(values: SimpleLens[G, Seq[Double]]): SimpleLens[G, Seq[Double]]
}

trait NoGenomeClamping <: GenomeClamping {
  override def clamp(values: SimpleLens[G, Seq[Double]]) = values
}

trait ClampedGenome <: GenomeClamping {
  override def clamp(values: SimpleLens[G, Seq[Double]]) =
    SimpleLens[G, Seq[Double]](values.get(_).map(tools.Math.clamp(_, 0.0, 1.0)), values.set)
}
