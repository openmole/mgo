/*
 * Copyright (C) 2012 reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.iscpif.mgo.termination

import fr.iscpif.mgo._

trait OrTermination extends Termination { self: Evolution =>
 
  def terminations: Traversable[Termination{type I = self.I}]
  def terminated(oldPop: IndexedSeq[I], newPop: IndexedSeq[I], step:Int) = 
    terminations.view.map{_.terminated(oldPop, newPop, step)}.exists(_ == true)
}
