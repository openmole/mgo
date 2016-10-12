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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._

object ZDT4PSE extends App {

  import algorithm.pse._
  import util.Random

  val pse = PSE(
    lambda = 10,
    phenotype = zdt4.compute,
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 200.0),
        definition = Vector(10, 10)),
    genomeSize = 10)

  val (finalState, finalPopulation) =
    run(pse).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new Random(42))

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

}
