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

object ZDT4PSE extends App {

  import algorithm.pse._

  val pse = PSE(
    lambda = 10,
    phenotype = zdt4.compute,
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 200.0),
        definition = Vector(10, 10)),
    genomeSize = 10)

  def evolution[M[_]: cats.Monad: StartTime: Random: Generation: IO: HitMapM] =
    pse.
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      evolution

  val (finalState, finalPopulation) = PSE(new util.Random(42)) { imp =>
    import imp._
    evolution[DSL].eval
  }

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

}
