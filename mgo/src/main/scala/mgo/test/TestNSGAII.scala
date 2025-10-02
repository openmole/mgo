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

import mgo.evolution._

object SphereNSGAII extends App {

  import algorithm._

  val nsga2: NSGA2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = (v, _) => Vector(sphere.compute(v)),
    continuous = sphere.genome(6))

  def evolution =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object DiscreteNSGAII extends App {

  import algorithm._

  val nsga2: NSGA2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = discreteSphere.compute,
    continuous = discreteSphere.continuous(6),
    discrete = discreteSphere.discrete(3))

  def evolution: RunAlgorithm[NSGA2, CDGenome.DeterministicIndividual.Individual[Vector[Double]], CDGenome.Genome, EvolutionState[Unit]] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object NoisySphereNSGAII extends App {

  import algorithm._

  val nsga2: NoisyNSGA2[Vector[Double]] =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = noisyDiscreteSphere.compute,
      aggregation = Aggregation.average,
      continuous = noisyDiscreteSphere.continuous(2),
      discrete = noisyDiscreteSphere.discrete(2))

  def evolution: RunAlgorithm[NoisyNSGA2[Vector[Double]], CDGenome.NoisyIndividual.Individual[Vector[Double]], CDGenome.Genome, NoisyNSGA2.NSGA2State] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NoisyNSGA2.result(nsga2, finalPopulation).mkString("\n"))

}

object ZDT4NSGAII extends App:

  import algorithm._

  val nsga2: NSGA2 =
    NSGA2(
      mu = 100,
      lambda = 100,
      fitness = zdt4.compute,
      continuous = zdt4.continuous(10))

  def evolution: RunAlgorithm[NSGA2, CDGenome.DeterministicIndividual.Individual[Vector[Double]], CDGenome.Genome, EvolutionState[Unit]] =
    nsga2.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))


object RastriginNSGAII extends App:

  import algorithm._

  val nsga2: NSGA2 = NSGA2(
    mu = 100,
    lambda = 100,
    fitness = (x, _) => Vector(rastrigin.compute(x)),
    continuous = rastrigin.continuous(10))

  def evolution =
    nsga2.
      until(afterGeneration(1000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA2.result(nsga2, finalPopulation).mkString("\n"))

