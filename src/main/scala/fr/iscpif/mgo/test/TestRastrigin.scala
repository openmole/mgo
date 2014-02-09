/*
 * Copyright (C) 09/02/14 Romain Reuillon
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
import scala.util.Random

object TestRastrigin extends App {
  val m =
    new Rastrigin with NSGAII with HierarchicalRanking {
      def genomeSize: Int = 6

      /** the size of the population */
      override def mu = 100

      /** Number of steps before the algorithm stops */
      override def steps = 200

      /** the size of the offspring */
      override def lambda = 100
    }

  implicit val rng = new Random

  val res =
    m.evolve.untilConverged {
      s =>
        println(s.generation + " " + s.individuals.map(_.fitness.values.head).min)
    }.individuals
}
