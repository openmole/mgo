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

import fr.iscpif.mgo._

import scala.util.Random

trait Griewangk <: GAProblem with MGFitness {
  def min = Seq.fill(genomeSize)(-600)
  def max = Seq.fill(genomeSize)(600)

  type P = Double

  def express(g: Seq[Double], rng: Random) =
    1.0 + g.map(x => math.pow(x, 2.0) / 4000).sum - g.zipWithIndex.map { case (x, i) => x / math.sqrt(i + 1.0) }.map(math.cos).reduce(_ * _)

  def evaluate(phenotype: P, rng: Random) = Seq(phenotype)
}
