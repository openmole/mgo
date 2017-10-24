/*
 * Copyright (C) 2012 Guillaume Chérel, Romain Reuillon
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

package mgo.test

import mgo._
import mgo.contexts._
import freedsl.dsl._

object SphereNSGAII extends App {

  import algorithm.nsga2._

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val nsga2 = NSGA2[M](
      mu = 100,
      lambda = 100,
      fitness = (v: Vector[Double]) => Vector(sphere.compute(v)),
      genomeSize = 2)

    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution
  }

  val (finalState, finalPopulation) =
    NSGA2(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(result(finalPopulation, sphere.scale).mkString("\n"))

}

object NoisySphereNSGAII extends App {

  import algorithm._
  import algorithm.noisynsga2._

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val nsga2 =
      NoisyNSGA2[M](
        mu = 100,
        lambda = 100,
        fitness = (rng: util.Random, v: Vector[Double]) => Vector(noisySphere.compute(rng, v)),
        aggregation = averageAggregation(_),
        genomeSize = 2)

    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  }

  val (finalState, finalPopulation) =
    NoisyNSGA2(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(result(finalPopulation, averageAggregation, noisySphere.scale).mkString("\n"))

}

object ZDT4NSGAII extends App {

  import algorithm.nsga2._

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val nsga2 =
      NSGA2[M](
        mu = 100,
        lambda = 100,
        fitness = zdt4.compute(_),
        genomeSize = 10)

    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution
  }

  val (finalState, finalPopulation) = NSGA2(new util.Random(42)) { impl =>
    import impl._
    evolution[DSL].eval
  }

}
