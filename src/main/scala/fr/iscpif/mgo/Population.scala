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
  def empty[G, P, F, MF]: Population[G, P, F, MF] = IndexedSeq.empty

  def age[G, P, F, MF](p: Population[G, P, F, MF]): Population[G, P, F, MF] = p.map(PopulationElement.age)
}

object PopulationElement {
  /**
   * Build a population element from an individual.
   *
   * @tparam G the genome type
   * @tparam F the fitness type
   * @tparam MF the meta-fitness type
   * @param i an individual
   * @param mf the meta-fitness of the individual in the population
   * @return a population element
   */
  def apply[G, P, F, MF](i: Individual[G, P, F], mf: MF) =
    new PopulationElement[G, P, F, MF](i.genome, i.phenotype, i.fitness, i.age, mf)

  def age[G, P, F, MF](pe: PopulationElement[G, P, F, MF]) =
    apply[G, P, F, MF](Individual.age(pe.toIndividual), pe.metaFitness)

}

/**
 * A population of solution
 *
 * @tparam G the genome type
 * @tparam MF the meta-fitness type
 */
trait Population[+G, +P, +F, +MF] {

  /** the content of the population */
  def content: Seq[PopulationElement[G, P, F, MF]]

  /** transform this population in a set of individual */
  def toIndividuals: Seq[Individual[G, P, F]] = content map { _.toIndividual }

  override def toString = content.toString
}

/**
 * An element of the population
 *
 * @tparam G the genome type
 * @tparam MF the meta-fitness type
 * @param genome the genome of the element
 * @param fitness the fitness evaluated for the genome
 * @param metaFitness the meta fitness of the element in the population
 */
class PopulationElement[+G, +P, +F, +MF](
  val genome: G,
  val phenotype: P,
  val fitness: F,
  val age: Long,
  val metaFitness: MF) {

  def toIndividual = Individual(genome, phenotype, fitness, age)

  override def toString = s"genome = $genome, phenotype = $phenotype, fitness = $fitness, ages = $age, metaFitness = $metaFitness)"
}
