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
  def empty[G, I]: Population[G, I] = IndexedSeq.empty
}

object PopulationElement {
  def apply[G, MF](i: Individual[G], mf: MF) = 
    new PopulationElement[G, MF](i.genome, i.fitness, mf)
}

/**
 * A population of solution
 * 
 * @tparam G the genome type
 * @tparam MF the meta-fitness type
 */
trait Population[+G, +MF] {

  /// the content of the population
  def content: IndexedSeq[PopulationElement[G, MF]]
  
  /// transform this population in a set of individual
  def toIndividuals: IndexedSeq[Individual[G]] = content map { _.toIndividual }
  
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
class PopulationElement[+G, +MF](val genome: G, val fitness: Fitness, val metaFitness: MF) {
  
  def individualFitness = fitness
  
  /// transform the population element in an individual
  def toIndividual = Individual(genome, individualFitness)
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", metaFitness = " + metaFitness + ")"
}