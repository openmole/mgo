/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import java.util.Random
import fr.iscpif.mgo.ga.operators.crossover.SBXBoundedCrossover
import fr.iscpif.mgo.ga.operators.mutation.CoEvolvingSigmaValuesMutation
import fr.iscpif.mgo.ga.selection.stochastic.BinaryTournamentNSGA2
import fr.iscpif.mgo._
import fr.iscpif.mgo.CrossOver
import fr.iscpif.mgo.Individual._
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection.Distance
import fr.iscpif.mgo.ga.selection.Ranking._
import fr.iscpif.mgo.ga.selection.ParetoRank
import fr.iscpif.mgo.ga.selection.Ranking
import fr.iscpif.mgo.ga.selection.Rank
import fr.iscpif.mgo.tools.Math
import ga._
import scala.annotation.tailrec
import selection.paretorankingstrategy.NonDominatedSorting
import termination.{AbstractTermination, SameRankingTermination}

object NSGAII {

  def buildIndividualsWithDistanceAndRanking[G <: GAGenome, FIT <: GAFitness](
                                                                               individuals: IndexedSeq[Individual[G, FIT]],
                                                                               dominance: Dominant,
                                                                               rank: Rank) = {
    val ranks = rank(individuals, dominance)
    val distances = Distance.crowding(individuals)
    (individuals zip ranks zip distances) map {
      case ((i, iranking), idistance) =>
        new Individual[G, FIT] with Distance with Ranking {
          val genome = i.genome
          val fitness = i.fitness
          val distance = idistance.distance
          val rank = iranking.rank
        }
    }
  }


  def sigma(
             _maxStep: Int,
             _archiveSize: Int,
             _genomeSize: Int,
             _factory: GAGenomeWithSigmaFactory,
             _evaluator: GAGenomeWithSigma => GAFitness,
             _sbxDistributionIndex: Double,
             _dominance: Dominant = new StrictDominant,
             _rank: Rank = new ParetoRank): NSGAII[GAGenomeWithSigma, GAGenomeWithSigmaFactory] = {

    type I = Individual[GAGenomeWithSigma, GAFitness] with Distance with Ranking

    new NSGAII[GAGenomeWithSigma, GAGenomeWithSigmaFactory]
      with BinaryTournamentNSGA2[I]
      with SameRankingTermination[I]
      with NonDominatedSorting[I]
      with CoEvolvingSigmaValuesMutation[GAGenomeWithSigma, GAGenomeWithSigmaFactory]
      with SBXBoundedCrossover[GAGenomeWithSigma, GAGenomeWithSigmaFactory]{

      def distributionIndex = _sbxDistributionIndex

      def factory = _factory

      def archiveSize = _archiveSize

      def maxStep = _maxStep

      def evaluator = _evaluator

      def dominance = _dominance

      def rank = _rank
    }
  }

}

// WORK NOTE :
/*
Selection occurs two times in the evolutionary loop.First, in order to generate offsprings,
parents must be selected from the current population (mating selection).Second, the new
parent population has to be selected from the offspring and the previous parents (Environmental selection)
A number of selection operators were proposed, which usually base the chance of selection of
particular individuals on their fitness values or their rank in the population, respectively.



// Environmental selection
// How to prevent non-dominated solutions from being lost?
// Environmental selection is used to obtain a representative efficient set

*/

// @fixme Refaire un check sur Ranking

trait NSGAII[G <: GAGenome, F <: GAGenomeFactory[G]]
  extends Evolution[G, F] with MOOElitism {

  type I = Individual[G, GAFitness] with Distance with Ranking

  def archiveSize: Int
  def envSelection(individuals: IndexedSeq[I]): IndexedSeq[I]

  override def evolve(population: IndexedSeq[I])(implicit aprng: Random): IndexedSeq[I] = {

    val offspring = breed(
      population,
      population.size
    ).map {
      g => Individual(g, evaluator)
    }

    val archive = population ++ offspring

    //Elitisme strategy
    val individuals = NSGAII.buildIndividualsWithDistanceAndRanking(archive, dominance, rank)
    envSelection(individuals)
  }

  def breed(archive: IndexedSeq[I],
            offSpringSize: Int)(implicit aprng: Random): IndexedSeq[G] = {

    //Crossover sur matingPopulation puis mutation
    def breed(acc: List[G] = List.empty): List[G] = {
      if (acc.size >= offSpringSize) acc
      else {
        val newIndividuals = crossover(selection(archive).genome, selection(archive).genome, factory).map {
          mutate(_, factory)
        }.take(offSpringSize).toIndexedSeq
        breed(acc ++ newIndividuals)
      }
    }

    breed().toIndexedSeq
  }


}
