/*
 * Copyright (C) 2025 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

import mgo.evolution.*
import mgo.evolution.algorithm.*

object ShpereOnePlusOneCMAES extends App:

  val cmaes = OnePlusOneCMAES(
    fitness = v => sphere.compute(v),
    continuous = sphere.genome(6)
  )

  def evolution =
    cmaes.
      until(afterGeneration(100000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(OnePlusOneCMAES.result(cmaes, finalPopulation).mkString("\n"))


object RastriginOnePlusOneCMAES extends App:

  import algorithm.*

  val cmaes = OnePlusOneCMAES(
    fitness = rastrigin.compute,
    continuous = rastrigin.continuous(10)
  )

  def evolution =
    cmaes.
      until(afterGeneration(100000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(OnePlusOneCMAES.result(cmaes, finalPopulation).mkString("\n"))


object RosenbrockOnePlusOneCMAES extends App:

  import algorithm._

  val cmaes = OnePlusOneCMAES(
    fitness = rosenbrock.compute,
    continuous = rosenbrock.continuous(40)
  )

  def evolution =
    cmaes.
      until(afterGeneration(100000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(OnePlusOneCMAES.result(cmaes, finalPopulation).mkString("\n"))


object ShpereMOCMAES extends App:

  val cmaes = MOCMAES(
    mu = 100,
    lambda = 100,
    fitness = v => Vector(sphere.compute(v)),
    continuous = sphere.genome(2)
  )

  def evolution =
    cmaes.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(MOCMAES.result(cmaes, finalPopulation).mkString("\n"))



object RastriginMOCMAES extends App:

  import algorithm._

  val cmaes = MOCMAES(
    mu = 100,
    lambda = 100,
    fitness = x => Vector(rastrigin.compute(x)),
    continuous = rastrigin.continuous(10)
  )

  def evolution =
    cmaes.
      until(afterGeneration(1000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(MOCMAES.result(cmaes, finalPopulation).mkString("\n"))


object RosenbrockMOCMAES extends App:

  import algorithm.*

  val cmaes = MOCMAES(
    mu = 100,
    lambda = 100,
    fitness = x => Vector(rosenbrock.compute(x)),
    continuous = rosenbrock.continuous(20),
    genomeDiversity = true
  )

  def evolution =
    cmaes.
      until(afterGeneration(1000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(MOCMAES.result(cmaes, finalPopulation).mkString("\n"))
