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

object Population {
  implicit def populationToSeq[G, P](p: Population[G, P]) = p.content
  implicit def traversableToPopulation[G, P](e: Traversable[Individual[G, P]]) = Population[G, P](e.toVector)

  /**
   * @tparam G the genome type
   * @tparam P the phenotype type
   * @return an empty population
   */
  def empty[G, P]: Population[G, P] = Population(Vector.empty)

  def apply[G, P](elements: Vector[Individual[G, P]]): Population[G, P] =
    new Population[G, P] {
      val content = elements
    }

}

/**
 * A population of solution
 *
 * @tparam G the genome type
 */
trait Population[+G, +P] { pop =>
  /** the content of the population */
  def content: Vector[Individual[G, P]]
  def age: Population[G, P] = Population(pop.content.map { Individual.age })
  override def toString = content.toString
}

object Individual {

  /**
   * Build an individual given a genome and an evaluation function
   *
   * @tparam G the type of the genome
   * @param g the value of the genome
   * @param expression the expression of the genome
   * @return the individual for the genome g
   */
  def apply[G, P](
    g: G,
    expression: (G => State[Random, P])): State[Random, Individual[G, P]] =
    for {
      _phenotype <- expression(g)
    } yield Individual[G, P](
      genome = g,
      phenotype = _phenotype
    )

  def age[G, P, F](i: Individual[G, P]): Individual[G, P] = i.copy(age = i.age + 1)
}

/**
 * An individual of the evolution
 */
case class Individual[+G, +P](genome: G, phenotype: P, age: Long = 0)
