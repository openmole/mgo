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
import fr.iscpif.mgo.algorithm.ga._
import fitness._
import genome._
import clone._

import scala.util.Random
import scalaz._

object SphereNSGAII extends App {

  def dimensions = 10
  def problem(g: GAGenome) = State { rng: Random => (rng, sphere(g.genomeValue)) }

  val evo =
    evolution(
      NSGA2[Double](
        mu = 10,
        fitness = Fitness(i => Seq(i.phenotype))
      )
    )(
        100,
        randomGenome(dimensions),
        problem,
        afterGeneration(100)
      )

  println(evo.eval(42).minBy(_.phenotype))

}

object StochasticSphereNSGAII extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val algo =
    noisyNSGA2[Double](
      mu = 100,
      fitness = Fitness(i => Seq(average(i.phenotype))),
      history = 100
    )

  def dimensions = 10
  def function = rastrigin

  def problem(g: GAGenome) = State { rng: Random =>
    val scaled = function.scale(g.genomeValue)
    val eval = function.compute(scaled)
    (rng, eval + (rng.nextGaussian() * 0.5 * math.sqrt(eval)))
  }

  val evo = evolution(algo)(100, randomGenome(dimensions), problem, afterGeneration(10000))

  import scala.Ordering.Implicits._
  val (s, res) = evo.run(47)

  val oldest = res.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))

  println(res.count(_.phenotype.size == oldest.phenotype.size))
  println(res.groupBy(_.genome.fromOperation).toList.map { case (k, v) => k -> v.size }.sortBy(_._1))
  println(function.scale(oldest.genomeValue) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size + " " + oldest.born)

}
