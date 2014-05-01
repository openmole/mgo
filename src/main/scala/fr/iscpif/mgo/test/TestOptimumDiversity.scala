/*
 * Copyright (C) 13/11/13 Romain Reuillon
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
import scalax.io.Resource

object TestOptimumDiversity extends App {

  val m = new Rastrigin with OptimumDiversity with CounterTermination {
    def genomeSize: Int = 2
    def lambda: Int = 200
    def mu = 200
    def archiveSize = 200

    def neighbours = 8
    def steps = 400

    def isGood(individual: Individual[G, P, F]) =
      individual.fitness.values.max < 8.05

    def individualPosition(individual: Individual[G, P, F]): Seq[Double] = Seq(individual.phenotype)

  }

  implicit val rng = new Random

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/novelty/novelty${s.generation}.csv")
      s.archive.foreach {
        i => output.append(i.genome.values.mkString(",") + "," + i.fitness.values.mkString(",") + "\n")
      }
      println(s.individuals.map(_.fitness.values.max).min)
    //println(s.generation + " " + s.archive.size)
  }

}
