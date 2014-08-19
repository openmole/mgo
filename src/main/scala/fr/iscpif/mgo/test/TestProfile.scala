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
import monocle.syntax._

object TestProfile extends App {
  val m =
    new Rastrigin with Profile with CounterTermination with ProfileGenomePlotter {
      def genomeSize: Int = 6
      def lambda: Int = 100
      def steps = 500
      def x: Int = 0
      def nX: Int = 1000
    }

  implicit val rng = newRNG(46)

  //  /*val output = Resource.fromFile(s"/tmp/generation.csv")
  //
  //  for {
  //    x <- -4.0 to 4.0 by 0.1
  //    y <- -4.0 to 4.0 by 0.2
  //  } {
  //    output.append(Seq(x, y, m.z(x, y)).mkString(",") + "\n")
  //  }*/
  //
  //  val archive = m.initialArchive
  //  val offspringGenomes = m.breed(Seq.empty, archive, 16)
  //
  //  //val rngs = (0 until offspringGenomes.size).map(_ => buildRNG(rng.nextLong))
  //
  //  val offspring = offspringGenomes.map { g => m.express(m.scale(g), rng) }
  //
  //  val output = Resource.fromFile(s"/tmp/S0.csv")
  //  (offspringGenomes zip offspring).foreach {
  //    case (g, f) =>
  //      val scaled = m.scale(g)
  //      output.append((m.values.get(scaled) ++ f).mkString(",") + "\n")
  //  }
  //
  //  val individuals = (offspringGenomes zip offspring).map {
  //    case (g, f) => Individual[m.G, m.P, m.F](g, f, f)
  //  }
  //
  //  val g2 = m.elitism(individuals, None)
  //
  //  def write(individuals: Seq[Individual[m.G, m.P, m.F]], file: String) = {
  //    val output2 = Resource.fromFile(file)
  //    individuals.foreach {
  //      i =>
  //        val scaled = m.scale(i)
  //        output2.append((m.values.get(scaled.genome) ++ scaled.fitness).mkString(",") + "\n")
  //    }
  //  }
  //
  //  write(g2, "/tmp/S2.csv")
  //
  //  val offspringGenomes2 = m.breed(g2, None, 16)
  //  val offspring2 = offspringGenomes2.map { g => m.express(m.scale(g), rng) }
  //
  //  val individuals2 = (offspringGenomes2 zip offspring2).map {
  //    case (g, f) => Individual[m.G, m.P, m.F](g, f, f)
  //  }
  //
  //  write(individuals2, "/tmp/S3.csv")

  val res =
    m.evolve.untilConverged {
      s =>
        println(s.generation)
    }.individuals

  val output = Resource.fromFile("/tmp/profile.csv")
  for {
    i <- res
    x = m.plot(i)
    v = m.aggregate(i.fitness)
    if !v.isPosInfinity
  } output.append(s"$x,$v\n")

}
