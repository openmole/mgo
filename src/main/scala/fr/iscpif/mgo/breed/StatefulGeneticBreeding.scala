/*
 * Copyright (C) Guillaume Chérel 2015
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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import util.Random
import fr.iscpif.mgo.genome.RandomGenome

/**
 * Stateful genetic breeding is used when the production of a new offspring depends on previous offsprings. Also allows post treatment of the offsprings
 * all together.
 */
trait StatefulGeneticBreeding <: Breeding with G with F with P {

  /** An alternative genome type (e.g. incomplete genome). Used when crossover and mutation produce a genome type that is subsequently altered at the postbreeding stage */
  type IG >: G
  /** A state that is passed to the selection, crossover and mutation methods and updated by each to convey the progression of the breeding. */
  type BreedingState

  def cloneProbability: Double = 0.0

  /**
   * Breed genomes from a population
   *
   * @param population the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Seq[G] = {

    val ibs = initialBreedingState(population, archive)

    val breeding: Iterator[(Seq[IG], BreedingState)] =
      Iterator.iterate((Seq.empty[IG], ibs)) {
        case (offsprings, s) =>
          val breeded: IG =
            if (population.isEmpty) initialGenome(s)
            else if (rng.nextDouble < cloneProbability) selection(population, archive, s).next().genome
            else {
              val Seq(g1, g2): Seq[G] = selection(population, archive, s).map { _.genome }.take(2).toSeq
              mutate(
                crossover(g1, g2, population, archive, s),
                population,
                archive,
                s)
            }
          (offsprings :+ breeded, updatedState(s, breeded))
      }

    val (offsprings, breedingstate) = breeding.drop(size - 1).next()

    postBreeding(population, offsprings, archive, breedingstate)
  }

  def initialGenome(s: BreedingState): IG

  def initialBreedingState(population: Population[G, P, F], archive: A): BreedingState

  /**Select an individual among the population.*/
  def selection(population: Population[G, P, F], archive: A, s: BreedingState)(implicit rng: Random): Iterator[Individual[G, P, F]]

  def crossover(g1: G, g2: G, population: Population[G, P, F], archive: A, s: BreedingState)(implicit rng: Random): IG

  def mutate(genome: IG, population: Population[G, P, F], archive: A, s: BreedingState)(implicit rng: Random): IG

  def updatedState(s: BreedingState, newoffspring: IG): BreedingState

  def postBreeding(population: Population[G, P, F], offsprings: Seq[IG], archive: A, s: BreedingState)(implicit rng: Random): Seq[G]

}
