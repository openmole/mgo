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
  def apply[G, P, F](g: G, expression: G => P, evaluation: (P, Random) => F)(implicit rng: Random) =
    new Individual[G, P, F] {
      val genome = g
      val phenotype = expression(genome)
      val fitness = evaluation(phenotype, rng)
      val age = 0L
    }

  /**
   * Build an individual from a genome and a fitness
   *
   * @tparam G the type of the genome
   * @param g the genmome
   * @param f the fitness
   * @return the individual
   */
  def apply[G, P, F](g: G, p: P, f: F, _age: Long = 0) =
    new Individual[G, P, F] {
      val genome = g
      val phenotype = p
      val fitness = f
      val age = _age
    }

  def age[G, P, F](i: Individual[G, P, F]) =
    new Individual[G, P, F] {
      val genome = i.genome
      val phenotype = i.phenotype
      val fitness = i.fitness
      val age = i.age + 1
    }

}

/**
 * An individual of the evolution
 */
trait Individual[+G, +P, +F] {
  /** the genome of this individual */
  def genome: G

  /** the phenotype of this individual */
  def phenotype: P

  /** the fitness evaluated for the genome */
  def fitness: F

  /** the number of generation of an individual */
  def age: Long

  /** transform this individual in a tuple genome, phenotype, fitness */
  //def toTuple = (genome, phenotype, fitness)

  override def toString = "(genome = " + genome.toString + ", phenotype = " + phenotype + ", fitness = " + fitness.toString + ", age = " + age + ")"
}
