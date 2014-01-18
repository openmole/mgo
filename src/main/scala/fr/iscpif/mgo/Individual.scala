/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo

import scala.util.Random

object Individual {

  implicit def individual2Fitness[F](i: Individual[_, _, F]) = i.fitness

  /**
   * Build an individual given a genome and an evaluation function
   *
   * @tparam G the type of the genome
   * @param g the value of the genome
   * @param expression the expression of the genome
   * @param evaluation the evaluation of the phenotype
   * @return the individual for the genome g
   */
  def apply[G, P, F](
    g: G,
    expression: G => P,
    evaluation: (P, Random) => F)(implicit rng: Random): Individual[G, P, F] = {

    val _phenotype = expression(g)
    val _fitness = evaluation(_phenotype, rng)

    Individual[G, P, F](
      genome = g,
      phenotype = _phenotype,
      fitness = _fitness
    )
  }

  def age[G, P, F](i: Individual[G, P, F]): Individual[G, P, F] = i.copy(age = i.age + 1)
}

/**
 * An individual of the evolution
 */
case class Individual[+G, +P, +F](genome: G, phenotype: P, fitness: F, age: Long = 0)