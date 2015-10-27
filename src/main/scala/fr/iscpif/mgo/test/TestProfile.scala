/*
 * Copyright (C) 08/01/13 Romain Reuillon
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo.algorithm._
import fr.iscpif.mgo._
import scalaz._
import util.Random

object TestProfileSphere extends App {

  val profile = new Profile {
    def lambda = 100
    def mu = 100
    val fitness = (_: Ind).phenotype
    type P = Double
    val plotter = genomeProfilePlotter(0, 100)
  }

  import profile._

  def dimensions = 10
  def problem(g: G): State[Random, P] = State { rng: Random => (rng, sphere(dimensions)(genomeValues.get(g).value)) }
  def termination: State[AlgorithmState, Boolean] = State { state => (state, state.generation >= 100) }

  val evo = evolution(profile)(randomGenome(dimensions), problem, termination)
  val res =
    evo.eval(42).content.map {
      i => s"${i.genome.values(0)}, ${i.phenotype}"
    }.mkString("\n")

  println(res)

}