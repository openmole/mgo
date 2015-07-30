/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools._
import genome.G
import util.Random
import scalaz._

/**
 * Layer of the cake for the mutation operation.
 */
trait Mutation <: G with P with F with A with BreedingContext {

  type Mutation = (G, Population[G, P, F], A, Random) => G
  def mutations: Vector[Mutation]

  /**
   * Mutate a genome
   *
   * @param genome genome to mutate
   * @param population the last computed population
   * @param archive the last archive
   * @param rng a random number generator
   * @return the mutated genome
   */
  def mutate(genome: G, population: Population[G, P, F], archive: A)(implicit rng: Random): State[BREEDINGCONTEXT, G] = ???
  //if (mutations.isEmpty) genome
  //else mutations.random(rng)(genome, population, archive, rng)

}
