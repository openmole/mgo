/*
 * Copyright (C) Guillaume Chérel 06/05/14
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

import mgo._
import mgo.contexts._
import freedsl.dsl._
import niche._

object ZDT4PSE extends App {

  import algorithm._
  import algorithm.PSE._

  val pse = PSE(
    lambda = 10,
    phenotype = zdt4.compute,
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 200.0),
        definition = Vector(10, 10)),
    continuous = zdt4.continuous(10))

  def evolution[M[_]: cats.Monad: StartTime: Random: Generation: IO: HitMap] =
    pse.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) = PSE.run(new util.Random(42)) { imp =>
    import imp._
    evolution[DSL].eval
  }

  println(result(pse, finalPopulation).mkString("\n"))

}

object ZDT4NoisyPSE extends App {

  import algorithm._
  import algorithm.NoisyPSE._

  val pse = NoisyPSE(
    lambda = 10,
    phenotype = (_, c, d) => zdt4.compute(c, d),
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 200.0),
        definition = Vector(10, 10)),
    continuous = zdt4.continuous(10),
    aggregation = averageAggregation(_))

  def evolution[M[_]: cats.Monad: StartTime: Random: Generation: IO: HitMap] =
    pse.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) = NoisyPSE.run(new util.Random(42)) { imp =>
    import imp._
    evolution[DSL].eval
  }

  println(result(pse, finalPopulation).mkString("\n"))

}