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
import scala.util.Random

/**
 * Layer to scale a sequence of Double in [0.0, 1.0]
 */
trait Scaling <: Problem with GA {

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
  def scale(x: Seq[Double]) =
    (x zip (min zip max)) map { case (x, (min, max)) => x scale (min, max) }

  override def evolve(implicit rng: Random): Iterator[EvolutionState] = evolve((g, rng) => express(scale(g), rng), evaluate)

  /**
   * Scale the genome from [0.0, 1.0] to the correct scale for the fitness
   * evaluation
   *
   * @param g the genome to scale
   * @return the scaled genome
   */
  def scale(g: G): G = values.mod(v => scale(v), g)

  /**
   * Scale a population element genome from [0.0, 1.0] to the correct scale
   *
   * @param i the population element to scale
   * @return the scaled population element
   */
  def scale[MF](i: PopulationElement[G, P, F, MF]): PopulationElement[G, P, F, MF] =
    i.copy(genome = scale(i.genome))

  def scale(i: Individual[G, P, F]): Individual[G, P, F] = i.copy(genome = scale(i.genome))

}
