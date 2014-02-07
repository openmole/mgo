/*
 * Copyright (C) 04/02/14 Romain Reuillon
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

package fr.iscpif.mgo.problem

import scala.util.Random
import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy

trait GAGenomePhenotype <: GenomePhenotype with GAProblem { pb =>
  /**
   * Compute the fitness for a point
   *
   * @param x the point to evaluate
   * @return the fitness of this point
   */
  def apply(x: Seq[Double], rng: Random): Seq[Double]

  /**
   * Compute the fitness value from a genome values.
   *
   * @param p the genome to evaluate
   * @return the fitness for this genome
   */
  def apply(p: P, rng: Random) = MGFitness(apply(values.get(p).toIndexedSeq, rng))

  def individualPosition(individual: Individual[G, P, F]): Seq[Double] = values.get(individual.genome)

}
