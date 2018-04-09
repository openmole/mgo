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

  import algorithm._

  val nsga2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = (v: Vector[Double], _) => Vector(sphere.compute(v)),
    continuous = sphere.genome(6))

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) =
    NSGA2.run(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object DiscreteNSGAII extends App {

  import algorithm._

  val nsga2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = discreteSphere.compute,
    continuous = discreteSphere.continuous(6),
    discrete = discreteSphere.discrete(3))

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) =
    NSGA2.run(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object NoisySphereNSGAII extends App {

  import algorithm._

  val nsga2 =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = noisyDiscreteSphere.compute,
      aggregation = averageAggregation(_),
      continuous = noisyDiscreteSphere.continuous(2),
      discrete = noisyDiscreteSphere.discrete(2))

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) =
    NoisyNSGA2.run(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(NoisyNSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object ZDT4NSGAII extends App {

  import algorithm._

  val nsga2 =
    NSGA2(
      mu = 100,
      lambda = 100,
      fitness = zdt4.compute,
      continuous = zdt4.continuous(10))

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) = NSGA2.run(new util.Random(42)) { impl =>
    import impl._
    evolution[DSL].eval
  }

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object RastriginNSGAII extends App {

  import algorithm._

  val nsga2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = (x, _) => Vector(rastrigin.compute(x)),
    continuous = rastrigin.continuous(2))

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] =
    nsga2.
      until(afterGeneration(1000)).
      trace { (s, is) => println(s.generation) }.
      evolution

  val (finalState, finalPopulation) =
    NSGA2.run(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))
}
