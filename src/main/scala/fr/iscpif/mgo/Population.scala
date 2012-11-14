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

object Population {
  /** 
   * @tparam G the genome type
   * @tparam F the fitness type
   * @tparam MF the meta-fitness type
   * @return an empty population
   */
  def empty[G, F, MF]: Population[G, F, MF] = IndexedSeq.empty
}

object PopulationElement {
  /** Build a population element from an individual.
   *
   * @tparam G the genome type
   * @tparam F the fitness type
   * @tparam MF the meta-fitness type
   * @param i an individual
   * @param mf the meta-fitness of the individual in the population
   * @return a population element
   */
  def apply[G, F, MF](i: Individual[G, F], mf: MF) =
    new PopulationElement[G, F, MF](i.genome, i.fitness, mf)
}

/**
 * A population of solution
 * 
 * @tparam G the genome type
 * @tparam MF the meta-fitness type
 */
trait Population[+G, +F, +MF] {

  /** the content of the population */
  def content: IndexedSeq[PopulationElement[G, F, MF]]
  
  /** transform this population in a set of individual */
  def toIndividuals: IndexedSeq[Individual[G, F]] = content map { _.toIndividual }
  
  override def toString = content.toString  
}

/**
 * An element of the population
 * 
 * @tparam G the genome type
 * @tparam MF the meta-fitness type
 * @param genome the genome of the element
 * @param fitness the fitness evaluated for the genome
 * @param metafitness the meta fitness of the element in the population
 */
class PopulationElement[+G, +F, +MF](val genome: G, val fitness: F, val metaFitness: MF) {
  
  /** The fitness of the original individual */
  def individualFitness = fitness
  
  /// transform the population element in an individual
  def toIndividual = Individual(genome, individualFitness)
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", metaFitness = " + metaFitness + ")"
}
