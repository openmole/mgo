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
import fr.iscpif.mgo.tools._
import scalaz._
import Scalaz._

import scala.language.higherKinds

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait GeneticBreeding <: Breeding with G with F with P with Mating with Cloning with Crossover with Mutation with BreedingContext {

  /**
   * Breed genomes from a population
   *
   * @param population the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Vector[G] = {
    val offsprings: Iterator[BreedingContext[Vector[G]]] =
      if (population.isEmpty) Iterator.continually(Vector(randomGenomeInContext).sequence[BreedingContext, G])
      else {
        val breeded: Iterator[BreedingContext[Vector[G]]] =
          // bind each result of mate to a breed action
          mate(population, archive).map { (_: BreedingContext[Vector[Individual[G, P, F]]]) >>= { breed(_, population, archive) } }

        val cloned: Iterator[BreedingContext[G]] =
          Iterator.continually(cloneInContext(population.toIndividuals.random))

        // breed or clone
        Iterator.continually {
          if (rng.nextDouble >= cloneProbability)
            breeded.next
          else
            cloned.next.map { Vector(_) }
        }
      }

    unwrapBreedingContext[Vector[G]](
      // Get the B in the outer position with sequence, then flatten the inner nested vectors with join
      (offsprings.take(size).toVector: Vector[BreedingContext[Vector[G]]]).sequence[BreedingContext, Vector[G]].map { (_: Vector[Vector[G]]).join })
  }

  def breed(
    individuals: Vector[Individual[G, P, F]],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): BreedingContext[Vector[G]] =
    /*implicitly[Monad[BreedingContext]].bind[Vector[G], Vector[G]](
      crossover[BreedingContext](individuals.map { _.genome }, population, archive))(
        parents => {
          implicitly[Monad[BreedingContext]].map {
            implicitly[Traverse[Vector]].traverse[BreedingContext, G, G](parents.toVector) { mutate[BreedingContext](_, population, archive) }
          } { _.toVector }
        })
    */
    crossover(individuals.map { _.genome }, population, archive) >>= { parents =>
      parents.traverse[BreedingContext, G] { mutate(_, population, archive) }
    }
}
