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

package fr.iscpif.mgo.test

import fr.iscpif.mgo.{ Population, Individual }
import fr.iscpif.mgo._
import scalaz.Lens
import scala.util.Random
import fr.iscpif.mgo.elitism.ModelFamilyElitism

object TestModelFamily extends App {

  /*val m = new RastriginVector with NSGAII with ModelFamilyElitism with MaxAggregation {

    override val masks: Seq[Seq[Boolean]] = (0 until 10).map(_ => RastriginVector.bitSet(10))

    override def genomeSize: Int = 11
    /** Number of steps before the algorithm stops */
    override def steps: Int = ???

    /** the size of the offspring */
    override def lambda: Int = ???

    override def nicheSize: Int = ???
  }*/
}
