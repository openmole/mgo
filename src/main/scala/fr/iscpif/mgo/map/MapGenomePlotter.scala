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

package fr.iscpif.mgo.map

import fr.iscpif.mgo._

trait MapGenomePlotter extends MapPlotter {

  type G <: GAGenome

  def x: Int
  def y: Int
  def nX: Int
  def nY: Int

  def plot(i: Individual[G, F]) = ((i.genome.values(x) * nX).toInt, (i.genome.values(y) * nY).toInt)

}
