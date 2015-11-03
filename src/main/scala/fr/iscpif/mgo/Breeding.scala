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

trait Breeding { this: Algorithm =>
  trait Selection <: State[AlgorithmState, Ind]
}

trait BreedingFunctions <: Breeding with Genome with Ranking with Diversity with Niche with Crossover with Mutation { this: Algorithm =>

  case class ChallengeResult[A](challenge: Vector[A])(implicit val ordering: scala.Ordering[A]) {
    def score(i: Int) = challenge(i)
  }

  trait Challenge[A] <: (Pop => State[AlgorithmState, ChallengeResult[A]]) { ch =>
    def and[B](other: Challenge[B]) = {
      new Challenge[(A, B)] {
        def apply(pop: Pop): State[AlgorithmState, ChallengeResult[(A, B)]] =
          for {
            c1 <- ch(pop)
            c2 <- other(pop)
          } yield {
            implicit val i1 = c1.ordering
            implicit val i2 = c2.ordering
            ChallengeResult(c1.challenge zip c2.challenge)
          }
      }
    }
  }

  def tournament[A](challenge: ChallengeResult[A], pop: Pop, rounds: (Int => Int) = _ => 1)(implicit random: monocle.Lens[AlgorithmState, Random]) = new Selection {

    override def apply(state: AlgorithmState): (AlgorithmState, Ind) = {
      def newChallenger: Int = random.get(state).nextInt(pop.size)

      @tailrec def round(champion: Int, rounds: Int): Int =
        if (rounds <= 0) champion
        else {
          val challenger = newChallenger
          import challenge.ordering._
          val newChampion = if (challenge.score(challenger) > challenge.score(champion)) challenger else champion
          round(newChampion, rounds - 1)
        }

      (state, pop(round(newChallenger, rounds(pop.size))))
    }
  }


  def onRank(implicit ranking: Ranking) = new Challenge[Lazy[Int]] {
    def apply(pop: Pop) = State.state {
      val ordering = implicitly[scala.Ordering[Lazy[Int]]]
      new ChallengeResult(ranking(pop))(ordering.reverse)
    }
  }

  def onDiversity(implicit diversity: Diversity, random: monocle.Lens[AlgorithmState, Random]) = new Challenge[Lazy[Double]] {
    override def apply(pop: Pop) =
      random.lifts { diversity(pop).map(div => new ChallengeResult(div)) }
  }

  def onHitCount[P](implicit hitMap: monocle.Lens[STATE, collection.Map[P, Int]], niche: Niche[P], stateLens: monocle.Lens[AlgorithmState, STATE]) = new Challenge[Int] {

    override def apply(pop: Pop) =
      for {
        hits <- get[AlgorithmState] map (stateLens composeLens hitMap).get
      } yield {
        val popHits = pop.map(i => hits(niche(i)))
        val ordering = implicitly[scala.Ordering[Int]]
        new ChallengeResult(popHits)(ordering.reverse)
      }

  }

  def randomSelection(population: Pop)(implicit random: monocle.Lens[AlgorithmState, Random]) = new Selection {
    override def apply(state: AlgorithmState): (AlgorithmState, Ind) =
      (state, population.random(random.get(state)))
  }



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


  def breedGenomes(
    selection: Selection,
    crossover: Crossover,
    mutation: Mutation) = {
    for {
      s1 <- selection
      s2 <- selection
      c <- crossover(s1.genome, s2.genome)
      (c1, c2) = c
      g1 <- mutation(c1)
      g2 <- mutation(c2)
    } yield { List(g2, g2) }
  }

}
