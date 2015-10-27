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

import tools.Math._

package object test {

  def sphere =
    (g: Seq[Double]) => g.map(_.scale(-2, 2)).map(x => x * x).sum

  def rastrigin =
    (g: Seq[Double]) => {
      val x = g.map(_.scale(-5.12, 5.12))
      10 * x.size + x.map(x => (x * x) - 10 * math.cos(2 * math.Pi * x)).sum
    }

}
