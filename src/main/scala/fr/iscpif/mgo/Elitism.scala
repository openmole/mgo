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

package fr.iscpif.mgo

import scala.annotation.tailrec
import scala.util.Random
import scalaz.Scalaz._
import scalaz._
import fitness._
import ranking._
import diversity._
import niche._

object elitism {
  import fitnessOld._
  import rankingOld._
  import nicheOld._
  import diversityOld._

  def merge[G, P](population: Population[Individual[G, P]], offspring: Population[Individual[G, P]]) = population ++ offspring

  object IsNaN {
    implicit def doubleCanBeNaN = new IsNaN[Double] {
      def apply(a: Double) = a.isNaN
    }

    implicit def seqOfACanBeNan[A](implicit isNaN: IsNaN[A]) = new IsNaN[Seq[A]] {
      override def apply(a: Seq[A]): Boolean = a.exists(isNaN)
    }
  }

  trait IsNaN[A] <: (A => Boolean)

  def removeNaN[G, P, A](population: Population[Individual[G, P]], fitness: Fitness[G, P, A])(implicit isNaN: IsNaN[A]) =
     population.filterNot(i => isNaN(fitness(i)))

  def keepNonDominated[G, P](mu: Int, ranking: Ranking[G, P], diversity: Diversity[G, P])(population: Population[Individual[G, P]]) = {
    def newPopulation: State[Random, Population[Individual[G, P]]] =
      if (population.size < mu) State.state { population }
      else {
        val ranks = ranking(population).map {
          _ ()
        }

        def sortedByRank =
          (ranks zip population).
            groupBy { case (r, _) => r }.
            toList.
            sortBy { case (r, _) => r }.
            map { case (_, v) => v.map { case (_, e) => e } }

        @tailrec def addFronts(fronts: List[Seq[Individual[G, P]]], acc: List[Individual[G, P]]): (Vector[Individual[G, P]], Vector[Individual[G, P]]) = {
          if (fronts.isEmpty) (Vector.empty, acc.toVector)
          else if (acc.size + fronts.head.size < mu) addFronts(fronts.tail, fronts.head.toList ::: acc)
          else (fronts.head.toVector, acc.toVector)
        }

        val (lastFront, selected) = addFronts(sortedByRank, List.empty)

        def mostDiverseOfLastFront =
          for {
            div <- diversity(lastFront)
          } yield {
            (lastFront zip div).
              sortBy { case (_, d) => d() }.
              reverse.
              slice(0, mu - selected.size).
              map { case (e, _) => e }
          }


        if (selected.size < mu) for { tail <- mostDiverseOfLastFront } yield selected ++ tail
        else State.state { selected }
      }

    newPopulation
  }


  def keepBestRanked[G, P, S](mu: Int, ranking: Ranking[G, P])(population: Population[Individual[G, P]]) =
    if (population.size < mu) population
    else {
      val ranks = ranking(population).map(_ ())
      (population zip ranks).sortBy {
        _._2
      }.map(_._1).take(mu)
    }

  def keepBest[G, P, S](mu: Int, fitness: Fitness[G, P, Double])(population: Population[Individual[G, P]])  =
    population.sortBy { fitness }.take(mu)

  def keepRandom[G, P, S](nicheSize: Int)(population: Population[Individual[G, P]]) =
    State { rng: Random => (rng, rng.shuffle(population).take(nicheSize)) }

  def nicheElitism[G, P, S, N](
    keep: Population[Individual[G, P]] => State[AlgorithmState[S], Population[Individual[G, P]]],
    population: Population[Individual[G, P]],
    niche: Niche[G, P, N])(implicit equal: Equal[N]): State[AlgorithmState[S], Population[Individual[G, P]]] =
    for {
      pops <- group(population.toList)(equal.contramap(niche)).traverseS { is => keep(is.to[Vector]) }
    } yield pops.flatten.toVector


  /*def conservativeFIFO(offspring: Pop, mu: Int)(implicit doubleFitness: DoubleFitness[F]) =  new Elitism {
    def conservativeFIFO(oldGeneration: Pop, candidate: Individual[G, P, F]): Pop =
      if (oldGeneration.size < 2) oldGeneration ++ Seq(candidate)
      else {
        val oldests = oldGeneration.zipWithIndex.groupBy { case (i, _) => i.age }.toSeq.sortBy { case (age, _) => age }.reverse.head._2
        val (oldest, oldestIndex) = oldests.sortBy { case (i, _) => doubleFitness(i.fitness) }.head

        val (concurrent, concurrentIndex) = oldGeneration.zipWithIndex.patch(oldestIndex, Seq.empty, 1).random
        if (doubleFitness(oldest.fitness) <= doubleFitness(concurrent.fitness)) oldGeneration.updated(concurrentIndex, candidate)
        else oldGeneration.updated(oldestIndex, candidate)
      }
    override def apply(s: EvolutionState) =
      s.copy(
        population =
          if (s.population.size < mu) s.population ++ offspring
          else offspring.foldLeft(s.population)(conservativeFIFO)
      )

  }*/


 /* //with DoubleFitness with NEATGenome with NEATArchive
  def NEATElitism(proportionKeep: Double, speciesKeptIfStagnate: Int, stagnationTimeThreshold: Int, offsprings: Population[G, P, F]) = new  Elitism {

    /**
      * Keep only the 20% fittest individuals of each species. If the fitness of the entire population does not
      * improve for more than 20 generations, only the two best species are allowed to reproduce.
      */
    def apply(evolutionState: EvolutionState): EvolutionState = {
      val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
        offsprings.content.groupBy { elt => elt.genome.species }
      //If the fitness of the entire population does not improve for more than 20 generations
      val lastfitnesses = archive.lastEntirePopulationFitnesses.takeRight(stagnationTimeThreshold)
      if ((lastfitnesses.length >= stagnationTimeThreshold) && (stagnationTimeThreshold > 0) &&
        //check for no improvement for the last generations
        !(lastfitnesses.dropRight(1) zip lastfitnesses.drop(1)).exists { case (f1, f2) => f2 > f1 }) {

        // Only allow the 2 best species to reproduce
        val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map { case (sp, indivs) => (sp, indivs.map { _.fitness }.sum / indivs.size) }.toSeq
        val bestspecies = speciesFitnesses.sortBy { case (sp, f) => -f }.take(speciesKeptIfStagnate).map { _._1 }
        bestspecies.flatMap { sp => keepBest(indivsBySpecies(sp)) }

      } else {
        indivsBySpecies.toSeq.flatMap { case (sp, indivs) => keepBest(indivs) }
      }
    }

    private def keepBest(offsprings: Seq[PopulationElement[G, P, F]]): Seq[PopulationElement[G, P, F]] = {
      //keep only a portion of the offsprings, but take at least one.
      val keep = offsprings.sortBy { elt: PopulationElement[G, P, F] => -elt.fitness }.take(max(1, round(proportionKeep * offsprings.length).toInt))
      keep
    }
  }*/



  /*def keepBestRanked[G, P, S](mu: Int, ranking: Ranking[G, P]) = new KeepInNiche[G, P, S]  {
    override def apply(population: Population[Individual[G, P]]) = State.state {
      val ranks = ranking(population).map(_())
      (population zip ranks).sortBy(_._2).unzip._1.take(mu)
    }
  }*/

  /*def keepBest[G, P, S](mu: Int, fitness: Fitness[G, P, Double]) = new KeepInNiche[G, P, S] {
    override def apply(population: Population[Individual[G, P]]) = State.state {
      (population).sortBy { fitness }.take(mu)
    }
  }*/




  /**
    * Based on http://sci2s.ugr.es/publications/ficheros/2005-SC-Lozano.pdf
    */
  /*trait DiversityAggregatedElitism <: Elitism with Aggregation with Mu with GA {

    def averageIntervalUsage(individuals: Population[G, P, F]): Double = {
      val transposedGenomes = individuals.map(i => values.get(i.genome)).transpose
      val mins = transposedGenomes.map(_.min)
      val maxs = transposedGenomes.map(_.max)
      (maxs zip mins).map { case (max, min) => max - min }.sum / genomeSize
    }

    override def computeElitism(oldGeneration: Population[G, P, F], offspring: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] =
      offspring.foldLeft(oldGeneration) {
        (population, candidate) =>
          if (population.size + 1 < mu) filter(population ++ Seq(candidate))
          else {
            val worsts =
              population.zipWithIndex.filter {
                case (i, _) => aggregate(i.fitness) >= aggregate(candidate.fitness)
              }

            if (worsts.isEmpty) population
            else {
              lazy val populationAIU = averageIntervalUsage(population)

              val (worstIndex, aiuDiff) =
                worsts.map {
                  case (_, index) =>
                    val modifiedPopulation = population.patch(index, Seq.empty, 1)
                    val modifiedPopulationAIU = averageIntervalUsage(modifiedPopulation)
                    val candidateAIUContribution = averageIntervalUsage(Seq(candidate) ++ modifiedPopulation) - modifiedPopulationAIU
                    val elementContribution = populationAIU - modifiedPopulationAIU
                    index -> (candidateAIUContribution - elementContribution)
                }.maxBy {
                  _._2
                }

              filter(
                if (aiuDiff > 0) population.updated(worstIndex, candidate)
                else population.sortBy(i => aggregate(i.fitness)).reverse.tail.toList ++ Seq(candidate)
              )
            }
          }
      }

  }*/


}
