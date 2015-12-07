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
import algorithm.ga._
import genome._
import fitness._
import niche._
import clone._

import scalaz._
import util.Random

//object Profile {
//  val deterministic =
//    profile[Double](
//      fitness = Fitness(_.phenotype),
//      niche = genomeProfile(0, 100)
//    )()
//
//
//  val stochastic =
//    profile[History[Double]](
//      fitness = Fitness(i => average(i.phenotype)),
//      niche = genomeProfile(0, 100)
//    )(cloneStrategy = queue(100))
//
//}
//
//object SphereProfile extends App {
//
//  def dimensions = 10
//  def problem(g: GAGenome) = State.gets { rng: Random => sphere(g.genomeValue) }
//
//  val evo = evolution(Profile.deterministic, 100)(randomGenome(dimensions), problem, afterGeneration(1000))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}
//
object StochasticSphereProfile extends App {

  import nicheOld._

  def niche = genomeProfile[GAGenome](0, 100)

  val stochastic =
    noisyProfile[Double](
      fitness = Fitness(i => average(i.phenotype)),
      niche = niche,
      nicheSize = 10,
      history = 100
    )

  def dimensions = 10
  def problem(g: GAGenome) = State.gets { rng: Random => sphere(g.genomeValue) + (rng.nextDouble() * 0.5) }

  val evo = evolution(stochastic)(100, randomGenome(dimensions), problem, afterGeneration(10000), s => println(s.common.generation))
  val res = evo.eval(42)

  val bests =
    for {
      p <- res.groupBy(i => niche(i)).map(_._2)
    } yield p.maxBy(_.phenotype.size)

  for { b <- bests } println(b.phenotype.size)
  for { b <- bests } println(s"""${b.genomeValue.mkString(",")},${average(b.phenotype)}""")

}
//
//object TestProfileRastrigin extends App {
//
//  import Profile.profile
//  import profile._
//
//  def dimensions = 10
//  def problem(g: G) = State { rng: Random => (rng, rastrigin(genomeValues.get(g))) }
//
//  val evo = evolution(profile, 100)(randomGenome(dimensions), problem, afterGeneration(100))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}