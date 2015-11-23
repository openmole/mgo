/*
 * Copyright (C) 2012 reuillon
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

package fr.iscpif.mgo

import scala.util.Random
import scalaz._
import Scalaz._

object Individual {

  /**
   * Build an individual given a genome and an evaluation function
   *
   * @tparam G the type of the genome
   * @param g the value of the genome
   * @param expression the expression of the genome
   * @return the individual for the genome g
   */
  def apply[G, P](g: G, expression: (G => State[Random, P])): State[CommonState, Individual[G, P]] =
    for {
      generation <- get[CommonState].map(_.generation)
      _phenotype <- CommonState.random.lifts(expression(g))
    } yield Individual[G, P](
      genome = g,
      phenotype = _phenotype,
      born = generation
    )
}

/**
 * An individual of the evolution
 */
case class Individual[+G, +P](genome: G, phenotype: P, born: Long)
