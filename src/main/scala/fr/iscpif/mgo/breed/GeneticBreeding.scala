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
import util.Random

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait GeneticBreeding <: Breeding with Lambda with G with F with P with Selection with CrossOver with Mutation with GA with Modifier {

  def cloneProbability: Double = 0.0

  /**
   * Breed genomes from a population
   *
   * @param individuals the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(individuals: Seq[Individual[G, P, F]], a: A, size: Int = lambda)(implicit aprng: Random): Seq[G] = {
    val population = toPopulation(individuals, a)

    val breeded =
      if (population.isEmpty) Iterator.continually(randomGenome)
      else
        for {
          Seq(i1, i2) <- selection(population).grouped(2)
          breed <- crossover(i1.genome, i2.genome, individuals, a).map { mutate(_, individuals, a) }
        } yield breed

    Iterator.continually {
      if (population.isEmpty || aprng.nextDouble >= cloneProbability) breeded.next()
      else selection(population).next().genome
    }.take(size).toIndexedSeq
  }

}
