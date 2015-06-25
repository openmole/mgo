/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

import fr.iscpif.mgo._
import util.Random
import scalax.io.Resource

object TestNSGAII extends App {

  val m =
    new ZDT4 with NSGAII with CounterTermination {
      def steps = 1000
      def mu = 200
      def lambda = 200
      def genomeSize = 1
    }

  implicit val rng = newRNG(42)

  val res =
    m.evolve.untilConverged {
      s => println(s.generation)
    }

  val output = Resource.fromFile("/tmp/res.csv")
  for {
    r <- res.population.toIndividuals
  } {
    def line = m.scale(m.values.get(r.genome)) ++ m.fitness(r)
    output.append(line.mkString(",") + "\n")
  }

}
