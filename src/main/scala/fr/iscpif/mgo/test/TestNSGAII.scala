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

object TestNSGAIIStochasticSphere extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val nsgaII = new NSGAII {
    def lambda = 100
    def mu = 100
    def fitness = Fitness(i => Seq(average(i.phenotype), -i.phenotype.size))
    override def mergeClones = cumulatePhenotype(100)
    type P = List[Double]
    override def cloneRate = 0.5

  }

  import nsgaII._

  def dimensions = 4
  def problem(g: G) = State { rng: Random => (rng, List(sphere(genomeValues.get(g).value) + (rng.nextGaussian() * 0.01))) }
  def termination = State { state: AlgorithmState => (state, state.generation >= 1000) }

  val evo = evolution(nsgaII)(randomGenome(dimensions), problem, termination)

  import scala.Ordering.Implicits._
  val oldest = evo.eval(42).content.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))
  println(oldest.genome.values.value.map(_.scale(-2, 2)) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size)
}
