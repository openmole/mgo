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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import fr.iscpif.mgo.selection.Mating
import util.Random
import fr.iscpif.mgo.genome.RandomGenome
import fr.iscpif.mgo.tools._

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait GeneticBreeding <: Breeding with G with F with P with Mating with Crossover with Mutation with RandomGenome {

  def cloneProbability: Double = 0.0

  /**
   * Breed genomes from a population
   *
   * @param population the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Seq[G] = {
    val breeded: Iterator[G] =
      if (population.isEmpty) Iterator.continually(randomGenome)
      else {
        def breedOrClone(mated: Seq[Individual[G, P, F]]) =
          breed(mated, population, archive).flatMap { breeded =>
            if (rng.nextDouble >= cloneProbability) Seq(breeded) else Seq(population.toIndividuals.random.genome, breeded)
          }

        for {
          mated <- mate(population, archive)
          breed <- breed(mated, population, archive)
        } yield if (rng.nextDouble >= cloneProbability) breed else population.toIndividuals.random.genome
      }

    Iterator.continually { breeded.next() }.take(size).toIndexedSeq
  }

  def breed(individuals: Seq[Individual[G, P, F]], population: Population[G, P, F], archive: A)(implicit rng: Random) =
    crossover(individuals.map(_.genome), population, archive).map { mutate(_, population, archive) }

}
