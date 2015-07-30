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
import scalaz._
import scalaz.std.vector._
import scalaz.syntax.monad._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait GeneticBreeding <: Breeding with G with F with P with Mating with Cloning with Crossover with Mutation with RandomGenome with BreedingContext {

  /**
   * Breed genomes from a population
   *
   * @param population the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Seq[G] = {
    val offsprings: Iterator[State[BREEDINGCONTEXT, Seq[G]]] =
      if (population.isEmpty) Iterator.continually(randomGenome)
      else {
        val breeded: Iterator[State[BREEDINGCONTEXT, Seq[G]]] =
          // bind each result of mate to a breed action
          //TODO: reformuler en parents >>= breed(_,population,archive) en utilisant les implicites de scalaz.syntax._
          mate(population, archive).map(MonadState[State, BREEDINGCONTEXT].bind(_)(breed(_, population, archive)))

        val cloned: Iterator[State[BREEDINGCONTEXT, Seq[G]]] =
          Iterator.continually(clone(population.toIndividuals.random))

        // breed or clone
        Iterator.continually {
          if (rng.nextDouble >= cloneProbability)
            breeded.next
          else
            cloned.next
        }
      }

    Traverse[Vector].sequenceS(offsprings.take(size).toVector).run(initBreedingContext)._2.flatten.toIndexedSeq
  }

  def breed(
    individuals: Seq[Individual[G, P, F]],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): State[BREEDINGCONTEXT, Seq[G]] =
    MonadState[State, BREEDINGCONTEXT].bind(
      crossover(individuals.map { _.genome }, population, archive))(
        parents => Traverse[Vector].traverseS(parents.toVector) { mutate(_, population, archive) }.map { _.toSeq })

}
