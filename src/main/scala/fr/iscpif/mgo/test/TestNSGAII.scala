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
import fr.iscpif.mgo.algorithm._

import scala.util.Random
import scalaz._
import Scalaz._

object TestNSGAIISphere extends App {

  val nsgaII = new NSGAII {
    def lambda = 100
    def mu = 100
    def fitness = Fitness(i => Seq(i.phenotype))
    type P = Double
  }

  import nsgaII._

  def dimensions = 10
  def problem(g: G): State[Random, P] = State { rng: Random => (rng, sphere(genomeValues.get(g))) }

  val evo = evolution(nsgaII)(randomGenome(dimensions), problem, afterStep(100))
  println(evo.eval(42).minBy(_.phenotype))

}

object TestNSGAIIStochastic extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val nsgaII = new NSGAII {
    def lambda = 100
    def mu = 100
    def fitness = Fitness(i => Seq(average(i.phenotype), -i.phenotype.size))
    val history = queue(100)
    override def cloneRate = 0.2
    type P = History[Double]
  }

  import nsgaII._

  def dimensions = 10
  def function = rastrigin

  def problem(g: G) = State { rng: Random =>
    val scaled = function.scale(g.genomeValue)
    val eval = function.compute(scaled)
    (rng, eval + (rng.nextGaussian() * 0.5 * math.sqrt(eval)))
  }

  val evo = evolution(nsgaII)(randomGenome(dimensions), problem, afterStep(1000))

  import scala.Ordering.Implicits._
  val (s, res) = evo.run(42)

  val oldest = res.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))

  println(res.count(_.phenotype.size == oldest.phenotype.size))
  println(function.scale(oldest.genome.values) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size + " " + oldest.born)

}
