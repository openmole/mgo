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

package fr.iscpif.mgo.problem

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.GAScaling
import scala.util.Random
import monocle.syntax._

/**
 * Layer to scale a sequence of Double in [0.0, 1.0]
 */
trait Scaling <: Problem with GA with GAScaling {

  /** minimum scaled value in the correct order */
  def min: Seq[Double]

  /** Maximum scaled value in the correct order */
  def max: Seq[Double]

  /**
   * Scale a vector according to the minimun and maximum
   *
   * @param x the vector to scale
   * @return the scaled vector
   */
  override def scale(x: Seq[Double]) =
    (x zip (min zip max)) map { case (x, (min, max)) => x scale (min, max) }

  /*override def evolve(implicit rng: Random): Iterator[EvolutionState] =
    evolve((g, rng) => express(scale(g), rng), evaluate)*/

}
