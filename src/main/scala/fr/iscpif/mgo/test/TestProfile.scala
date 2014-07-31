/*
 * Copyright (C) 08/01/13 Romain Reuillon
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import util.Random
import scalax.io._

object TestProfile extends App {
  val m =
    new Himmelblau with Profile with CounterTermination with ProfileGenomePlotter {
      //def genomeSize: Int = 6
      def lambda: Int = 200
      def steps = 200
      def x: Int = 0
      def nX: Int = 1000
    }

  implicit val rng = newRNG(42)

  val res = m.evolve.untilConverged(s => println(s.generation)).individuals

  val output = Resource.fromFile("/tmp/matrix2.csv")
  for {
    i <- res
    x = m.plot(i)
    v = m.aggregate(i.fitness)
    if !v.isPosInfinity
  } output.append(s"$x,$v\n")

}
