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

  implicit val rng = new Random

  val nsga2 =
    new Rastrigin with NSGAII with HierarchicalRanking {
      def mu = 100
      def lambda = 100
      def genomeSize = 20
      def steps = 1000
    }

  val res =
    nsga2.evolve.untilConverged {
      s =>
        println(s.individuals.map(_.fitness))
        println(s.generation)
    }.individuals

  val output = Resource.fromFile("/tmp/res.csv")
  for {
    r <- res
  } {
    def line = nsga2.scale(nsga2.values.get(r.genome)) ++ nsga2.fitness.get(r.fitness)
    output.append(line.mkString(",") + "\n")
  }

}
