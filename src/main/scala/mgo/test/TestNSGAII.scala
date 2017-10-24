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

  import algorithm.noisynsga2._

  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  val nsga2 =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = (rng, v) => Vector(noisySphere.compute(rng, v)),
      aggregation = aggregation,
      genomeSize = 2)

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) =
    NoisyNSGA2(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(result(finalPopulation, aggregation, noisySphere.scale).mkString("\n"))

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

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

}

object DropWaveNSGAII extends App {

  import breeding._
  import algorithm._
  import algorithm.nsga2._
  import better.files._

  val directory = File("/tmp/dropwave/")
  directory.createDirectories()

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val (nsga2, source) = sourceOf {
      NSGA2[M](
        mu = 50,
        lambda = 50,
        fitness = (x: Vector[Double]) => Vector(dropWave.compute(x)),
        genomeSize = 2,
        operators = ManualOperators[M](sbxC(0.5), bga(mutationRate = _ => 1.0, mutationRange = 0.1))
      )
    }

    def save(generation: Long, population: Vector[Individual]) = {
      val lines = result(population, dropWave.scale).map { case (g, v) => g ++ v }.map(_.mkString(","))
      (directory / s"$generation.csv") overwrite lines.mkString("\n")
    }

    (directory / "source") overwrite source

    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => save(s.generation, is)).
      evolution
  }

  val (finalState, finalPopulation) = NSGA2(new util.Random(42)) { impl =>
    import impl._
    evolution[DSL].eval
  }

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

}