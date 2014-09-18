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
import tools.Math._

import scala.util.Random
import scalax.io.Resource

object TestAggregated extends App {

  implicit val rng = newRNG(42)

  //val mutationRes = Resource.fromFile("/tmp/mutation.csv")
  //val crossoverRes = Resource.fromFile("/tmp/crossover.csv")

  val m =
    new Rastrigin with AggregatedOptimisation with DiversityAggregatedElitism {
      def mu = 200
      def lambda = 1
      def genomeSize = 10
      def steps = 100000

      override type STATE = None.type
      override def terminated(population: Population[G, P, F], terminationState: STATE)(implicit rng: Random): (Boolean, STATE) = {
        (population.map(i => aggregate(i.fitness)).min <= 0.0, None)
      }

      override def initialState: STATE = None
    }

  /* m.evolve.untilConverged {
    i =>
      println(i.population.map(i => m.aggregate(i.fitness)).min)

      val cstats = m.crossoverStats(i.population).toMap.withDefaultValue(0.0)
      crossoverRes.append((0 until m.crossovers.size).map(cstats).mkString(",") + "\n")

      val mstats = m.mutationStats(i.population).toMap.withDefaultValue(0.0)
      mutationRes.append((0 until m.mutations.size).map(mstats).mkString(",") + "\n")
  }*/

  m.evolve.untilConverged {
    i => println(i.generation + " " + i.population.map(i => m.aggregate(i.fitness)).min)
  }
  /*println(average((0 until 100).par.map(i => m.evolve.untilConverged { i => println(i.generation) }.generation.toDouble).seq))*/

  /*val res =
    m.evolve.untilConverged {
      s =>
        println(s.generation + " " + s.population.map(i => m.aggregate(i.fitness)).min)
    }.population.toIndividuals

  val output = Resource.fromFile("/tmp/res.csv")
  for {
    r <- res
  } {
    def line = m.scale(m.values.get(r.genome)) ++ m.fitness(r)
    output.append(line.mkString(",") + "\n")
  }*/

}
