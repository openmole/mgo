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
import genome.{ GenomeFactory, G }
import java.util.Random

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait Breeding extends Lambda with G with F with P with MF with Selection with CrossOver with Mutation with GenomeFactory with Modifier {

  /**
   * Breed genomes from a population
   *
   * @param individuals the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(individuals: Seq[Individual[G, P, F]], a: A, size: Int = lambda)(implicit aprng: Random): Seq[G] = {
    val population = toPopulation(individuals, a)
    Iterator.continually {
      if (population.isEmpty) IndexedSeq(genomeFactory.random)
      else crossover(selection(population).genome, selection(population).genome).map { mutate(_) }
    }.flatten.take(size).toIndexedSeq
  }

}
