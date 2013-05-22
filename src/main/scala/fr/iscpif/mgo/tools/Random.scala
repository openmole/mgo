/*
 *  Copyright (C) 2010 Romain Reuillon <romain.reuillon at openmole.org>
 * 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.tools

import java.util.{ Random => JRandom }
import scala.util.{ Random => SRandom }

object Random {

  implicit class SeqDecorator[T](elts: Seq[T]) {
    def random(implicit prng: SRandom) = elts(prng.nextInt(elts.size))
  }

  def rndmChoice[T](t1: T, t2: T)(implicit rng: SRandom): T = {
    if (rng.nextDouble < 0.5) t1 else t2
  }

}
