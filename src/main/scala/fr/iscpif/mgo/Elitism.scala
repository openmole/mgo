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
import scalaz.Scalaz._
import scalaz._

/**
 * Cake layer to eliminated elements of a population
 */
trait Elitism <: Pop  { this: Algorithm =>
  def Elitism(f: (AlgorithmState => (AlgorithmState, Pop))) = State(f)
}


trait ElitismDefault <: Elitism with Fitness with Niche with Ranking with Diversity { this: Algorithm =>

  trait KeepInNiche extends (Pop => State[AlgorithmState, Pop])

  def merge(population: Pop, offspring: Pop) = State[AlgorithmState, Pop] { s =>
    (s, population ++ offspring)
  }

  def removeClone(population: Pop)(implicit genomeEquality: Equal[G]) = Elitism { s =>
      def newPop =
        groupWhen(population.toList)((i1, i2) => genomeEquality.equal(i1.genome, i2.genome)).map {
          _.sortBy(_.age).head
        }

      (s, newPop)
    }

  def removeNaN(population: Pop)(implicit mg: Fitness[Seq[Double]]) = Elitism { s =>
      def newPop = population.filterNot(i => mg(i).exists(_.isNaN))
      (s, newPop)
    }


  def keepNonDominated(mu: Int, population: Pop)(implicit ranking: Ranking, diversity: Diversity) = Elitism { s =>
    def newPopulation: Pop =
        if (population.content.size < mu) population
        else {
          val ranks = ranking(population).map {
            _ ()
          }

          def sortedByRank =
            (ranks zip population.content).
              groupBy { case (r, _) => r }.
              toList.
              sortBy { case (r, _) => r }.
              map { case (_, v) => v.map { case (_, e) => e } }

          @tailrec def addFronts(fronts: List[Seq[Individual[G, P]]], acc: List[Individual[G, P]]): (Seq[Individual[G, P]], Seq[Individual[G, P]]) = {
            if (fronts.isEmpty) (Seq.empty, acc)
            else if (acc.size + fronts.head.size < mu) addFronts(fronts.tail, fronts.head.toList ::: acc)
            else (fronts.head, acc)
          }

          val (lastFront, selected) = addFronts(sortedByRank, List.empty)

          if (selected.size < mu) {
            selected ++
              (lastFront zip diversity(lastFront).eval(s.random)).
                sortBy { case (_, d) => d() }.
                reverse.
                slice(0, mu - selected.size).
                map { case (e, _) => e }
          } else selected
        }

      (s, newPopulation)
    }


  def keepBest(mu: Int, population: Pop)(implicit ranking: Ranking) = Elitism { s =>
      def newPopulation: Pop =
        if (population.size < mu) population
        else {
          val ranks = ranking(population).map(_())
          (population.content zip ranks).sortBy { _._2 }.map(_._1).take(mu)
        }
      (s, newPopulation)
    }



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

  def keepRandom(nicheSize: Int) = new KeepInNiche {
    override def apply(population: Pop) = State[AlgorithmState, Pop] {
      s => (s, s.random.shuffle(population.content).take(nicheSize))
    }
  }

  def keepBestRanked(mu: Int)(implicit ranking: Ranking) = new KeepInNiche  {
    override def apply(population: Pop) = State.state {
      val ranks = ranking(population).map(_())
      (population.content zip ranks).sortBy(_._2).unzip._1.take(mu)
    }
  }

  def nicheElitism(keep: KeepInNiche, population: Pop)(implicit niche: Niche[Any]) = Elitism { s =>
    def composition =
      for {
        pops <- population.content.groupBy(niche).toList.traverseS { case (_, v) => keep(v) }
        pop <- State[AlgorithmState, Pop] { s => (s, pops.flatten) }
      } yield pop
    composition.run(s)
  }

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
