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

import util.Random
import math._
import monocle.syntax._

trait Rosenbrock <: GAProblem with MGFitness {
  def genomeSize: Int = 2

  def min = Seq.fill(2)(-2048.0)
  def max = Seq.fill(2)(2048.0)

  type P = Double

  def express(g: Seq[Double], rng: Random) = {
    val Seq(x, y) = g
    pow(1 - x, 2) + 100 * pow(y - pow(x, 2), 2)
  }

  def evaluate(x: Double, rng: Random) = Seq(x)

}
