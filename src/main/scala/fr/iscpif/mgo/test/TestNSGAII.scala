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
  def problem(g: G): State[Random, P] = State { rng: Random => (rng, sphere(genomeValues.get(g).value)) }
  def termination: State[AlgorithmState, Boolean] = State { state => (state, state.generation >= 100) }

  val evo = evolution(nsgaII)(randomGenome(dimensions), problem, termination)
  println(evo.eval(42).content.minBy(_.phenotype))

}

object TestNSGAIIStochastic extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val nsgaII = new NSGAII {
    def lambda = 100
    def mu = 100
    def fitness = Fitness(i => Seq(average(i.phenotype), -i.phenotype.size))
    override def mergeClones = cumulatePhenotype(100)
    override def cloneRate = 0.1
    //def fitness = Fitness(i => Seq(average(i.phenotype)))
    type P = List[Double]

  }

  import nsgaII._

  def dimensions = 4
  def function = rastrigin

  def problem(g: G) = State { rng: Random =>
    val scaled = function.scale(g.genomeValue)
    val eval = function.compute(scaled)
    (rng, List(eval + (rng.nextGaussian() * 0.5 * math.sqrt(eval))))
  }

  def termination = State { state: AlgorithmState => (state, state.generation >= 10000) }

  val evo = evolution(nsgaII)(randomGenome(dimensions), problem, termination)

  import scala.Ordering.Implicits._
  val (s, res) = evo.run(42)

  val oldest = res.content.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))

  (ranking.apply(res) zip res.content).foreach(println)

  println(res.content.count(_.phenotype.size == oldest.phenotype.size))
  println(function.scale(oldest.genome.values) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size)

  //
}
