/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

import fr.iscpif.mgo.tools._

import scala.annotation.tailrec
import scala.util.Random
import scalaz._
import Scalaz._
import ranking._
import niche._
import crossover._
import mutation._
import diversity._

object breeding {
  import rankingOld._
  import nicheOld._
  import diversityOld._

  case class ChallengeResult[A](challenge: Vector[A])(implicit val ordering: scala.Ordering[A]) {
    def score(i: Int) = challenge(i)
    def and[B](other: ChallengeResult[B]): ChallengeResult[(A, B)] = {
      implicit val i2 = other.ordering
      ChallengeResult(challenge zip other.challenge)
    }
  }

  type Challenge[G, P, S, A] = (Population[Individual[G, P]] => State[S, ChallengeResult[A]])

  def tournament[G, P, A](challenge: ChallengeResult[A], pop: Population[Individual[G, P]], rounds: (Int => Int) = _ => 1) = State { rng: Random =>
    def newChallenger: Int = rng.nextInt(pop.size)

    @tailrec def round(champion: Int, rounds: Int): Int =
      if (rounds <= 0) champion
      else {
        val challenger = newChallenger
        import challenge.ordering._
        val newChampion = if (challenge.score(challenger) > challenge.score(champion)) challenger else champion
        round(newChampion, rounds - 1)
      }

    (rng, pop(round(newChallenger, rounds(pop.size))))
  }

  def onRank[G, P](ranking: Ranking[G, P]) = new Challenge[G, P, Random, Lazy[Int]] {
    def apply(pop: Population[Individual[G, P]]) = State.state {
      val ordering = implicitly[scala.Ordering[Lazy[Int]]]
      new ChallengeResult(ranking(pop))(ordering.reverse)
    }
  }

  def onDiversity[G, P](diversity: Diversity[G, P]) = new Challenge[G, P, Random, Lazy[Double]] {
    override def apply(pop: Population[Individual[G, P]]) = diversity(pop).map(div => new ChallengeResult(div))
  }

  def onHitCount[G, P, S, N](hitMap: monocle.Lens[S, collection.Map[N, Int]], niche: Niche[G, P, N]) = new Challenge[G, P, AlgorithmState[S], Int] {

    override def apply(pop: Population[Individual[G, P]]) =
      for {
        hits <- state[S] map hitMap.get
      } yield {
        val popHits = pop.map(i => hits(niche(i)))
        val ordering = implicitly[scala.Ordering[Int]]
        new ChallengeResult(popHits)(ordering.reverse)
      }

  }

  def randomSelection[G, P, S](population: Population[Individual[G, P]]) =
    State { rng: Random => (rng, population.random(rng)) }

  /*trait NEATMating <: Mating with Lambda with BreedingContext with NEATGenome with P with F {

    override def mate(population: Population[G, P, F], archive: A)(implicit rng: Random): Iterator[BreedingContext[Vector[Individual[G, P, F]]]] =
      selection(population, archive, lambda).map { couple => Vector(couple._1, couple._2).point[BreedingContext] }

    /** returns pairs of parents from the population */
    def selection(
                   population: Population[G, P, F],
                   archive: A,
                   size: Int)(implicit rng: Random): Iterator[(Individual[G, P, F], Individual[G, P, F])] = {
      val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
        population.toIndividuals.groupBy { indiv => indiv.genome.species }
      val result = speciesOffsprings(indivsBySpecies, size).flatMap {
        case (species, nb) =>
          Iterator.fill(nb) {
            val nparents = indivsBySpecies(species).length
            val p1 = indivsBySpecies(species)(rng.nextInt(nparents))
            val p2 =
            // Have a chance to mate between different species
              if ((rng.nextDouble() < interSpeciesMatingProb) && (indivsBySpecies.size > 1)) {
                val otherSpecies = indivsBySpecies.keys.filter { _ != species }.toSeq(rng.nextInt(indivsBySpecies.size - 1))
                val nindivsOtherSpecies = indivsBySpecies(otherSpecies).length
                indivsBySpecies(otherSpecies)(rng.nextInt(nindivsOtherSpecies))
              } else indivsBySpecies(species)(rng.nextInt(nparents))
            //println(s"sampling species (${p1.genome.species}, ${p2.genome.species})")
            (p1, p2)
          }
      }

      result.toIterator
    }

    /** Returns tuples (species, number of offsprings) */
    def speciesOffsprings(
                           indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
                           totalOffsprings: Int): Vector[(Int, Int)]

    def interSpeciesMatingProb: Double

  }*/

  def breed[G, P, S](
    crossover: Crossover[G, S],
    mutation: Mutation[G, S])(s1: Individual[G, P], s2: Individual[G, P]): State[AlgorithmState[S], List[G]] =
    for {
      c <- crossover(s1.genome, s2.genome)
      (c1, c2) = c
      g1 <- mutation(c1)
      g2 <- mutation(c2)
    } yield { List(g1, g2) }

}
