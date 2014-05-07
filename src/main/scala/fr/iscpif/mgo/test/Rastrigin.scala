/*
 * Copyright (C) 20/11/12 Romain Reuillon
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

import fr.iscpif.mgo._
import scala.util.Random

object Rastrigin {
  def value(x: Seq[Double]) = 10 * x.size + x.map(x => (x * x) - 10 * math.cos(2 * math.Pi * x)).sum
}

trait Rastrigin <: GAProblem with MGFitness {
  def min = Seq.fill(genomeSize)(-5.12)
  def max = Seq.fill(genomeSize)(5.12)

  type P = Double

  override def express(g: G, rng: Random) = Rastrigin.value(values.get(g))
  override def evaluate(phenotype: P, rng: Random) = Seq(phenotype)
}
