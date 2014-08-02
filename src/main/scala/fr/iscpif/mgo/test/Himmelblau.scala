/*
 * Copyright (C) 2014 Romain Reuillon
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

import fr.iscpif.mgo.fitness.MGFitness
import fr.iscpif.mgo.problem.GAProblem

import scala.math._
import scala.util.Random
import monocle.syntax._

trait Himmelblau <: GAProblem with MGFitness {
  def genomeSize: Int = 2

  def min = List(-4.5, -4.5)
  def max = List(4.5, 4.5)

  type P = Seq[Double]

  def z(x: Double, y: Double) = {
    pow(pow(x, 2) + y - 11, 2) + pow(x + pow(y, 2) - 7, 2)
  }

  override def express(g: G, rng: Random) = {
    val Seq(x, y) = (g |-> values get)
    Seq(z(x, y))
  }

  def evaluate(x: Seq[Double], rng: Random) = x

}

