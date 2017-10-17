/*
 * Copyright (C) 08/01/13 Guillaume Chérel, Romain Reuillon
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

package mgo.test

import mgo._
import mgo.contexts._
import freedsl.dsl._

object SphereProfile extends App {

  import algorithm.profile._

  //Profile the first dimension of the genome
  val algo = Profile(
    lambda = 100,
    fitness = sphere.compute,
    niche = genomeProfile(x = 0, nX = 10),
    genomeSize = 10)

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    algo.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).evolution

  val (finalState, finalPopulation) =
    Profile(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(
    result(finalPopulation, sphere.scale).map {
      case (v, f) => (v ++ Vector(f)).mkString(",")
    }.mkString("\n")
  )

}

object NoisySphereProfile extends App {

  import algorithm.noisyprofile._

  def aggregation(history: Vector[Double]) = history.sum / history.size
  def niche = genomeProfile(x = 0, nX = 10)

  val algo = NoisyProfile(
    muByNiche = 20,
    lambda = 100,
    fitness = noisySphere.compute,
    aggregation = aggregation,
    niche = niche,
    genomeSize = 5)

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    algo.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) =
    NoisyProfile(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(
    result(finalPopulation, aggregation, noisySphere.scale, niche).map {
      case (v, f) => (v ++ Vector(f)).mkString(",")
    }.mkString("\n")
  )

}
