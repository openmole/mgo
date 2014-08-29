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
   * @return an empty population
   */
  def empty[G, P, F]: Population[G, P, F] = IndexedSeq.empty

  def apply[G, P, F](elements: Seq[PopulationElement[G, P, F]]): Population[G, P, F] =
    new Population[G, P, F] {
      lazy val content = elements
    }

  def fromIndividuals[G, P, F](individuals: Seq[Individual[G, P, F]]): Population[G, P, F] = Population(individuals.map { PopulationElement(_) })

}

object PopulationElement {
  /**
   * Build a population element from an individual.
   *
   * @tparam G the genome type
   * @tparam F the fitness type
   * @param i an individual
   * @return a population element
   */
  def apply[G, P, F](i: Individual[G, P, F]) =
    new PopulationElement[G, P, F](i.genome, i.phenotype, i.fitness, i.age)

  def age[G, P, F](pe: PopulationElement[G, P, F]) =
    apply[G, P, F](Individual.age(pe.toIndividual))

}

/**
 * A population of solution
 *
 * @tparam G the genome type
 */
trait Population[+G, +P, +F] { pop =>

  /** the content of the population */
  def content: Seq[PopulationElement[G, P, F]]

  def age: Population[G, P, F] =
    new Population[G, P, F] {
      lazy val content = pop.content.map { PopulationElement.age }
    }

  /** transform this population in a set of individual */
  def toIndividuals: Seq[Individual[G, P, F]] = content map { _.toIndividual }

  override def toString = content.toString
}

/**
 * An element of the population
 *
 * @tparam G the genome type
 * @param genome the genome of the element
 * @param fitness the fitness evaluated for the genome
 */
case class PopulationElement[+G, +P, +F](
    genome: G,
    phenotype: P,
    fitness: F,
    age: Long) {

  def toIndividual = Individual(genome, phenotype, fitness, age)

  override def toString = s"genome = $genome, phenotype = $phenotype, fitness = $fitness, ages = $age)"
}
