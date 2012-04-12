/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import java.util.Random
import fr.iscpif.mgo.ga._
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
import scala.annotation.tailrec
import termination.{AbstractTermination, SameRankingTermination}
import selection.paretorankingstrategy.NonDominatedSorting

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
             maxStep: Int,
             archiveSize: Int,
             factory: GAGenomeWithSigmaFactory,
             evaluator: GAGenomeWithSigma => GAFitness,
             sbxDistributionIndex: Double,
             dominance: Dominant = new StrictDominant,
             rank: Rank = new ParetoRank): NSGAII[GAGenomeWithSigma, GAGenomeWithSigmaFactory] = {
    def _dominance = dominance
    def _rank = rank


    /*
    Selection occurs two times in the evolutionary loop.First, in order to generate offsprings,
    parents must be selected from the current population (mating selection).Second, the new
    parent population has to be selected from the offspring and the previous parents (Environmental selection)
    A number of selection operators were proposed, which usually base the chance of selection of
    particular individuals on their fitness values or their rank in the population, respectively.
     */


    // Environmental selection
    // How to prevent non-dominated solutions from being lost?
    // Environmental selection is used to obtain a representative efficient set

    new NSGAII[GAGenomeWithSigma, GAGenomeWithSigmaFactory](maxStep, archiveSize, factory, evaluator) {
      def mutationOperator = new CoEvolvingSigmaValuesMutation[GAGenomeWithSigma, GAGenomeWithSigmaFactory]

      def crossoverOperator = new SBXBoundedCrossover[GAGenomeWithSigma, GAGenomeWithSigmaFactory](sbxDistributionIndex)

      def dominance = _dominance

      def rank = _rank
    }
  }

  def elitism[G <: GAGenome](archive: IndexedSeq[Individual[G, GAFitness]],
                             envSelection: NonDominatedSorting,
                             rank: Rank = new ParetoRank
                              )(implicit dominance: Dominant): IndexedSeq[Individual[G, GAFitness] with Distance with Ranking] = {

    val individuals = buildIndividualsWithDistanceAndRanking(archive, dominance, rank)
    envSelection.apply(individuals)(dominance)
  }

  def breed[G <: GAGenome, F <: GAGenomeFactory[G], I <: Individual[G, GAFitness] with Distance with Ranking](archive: IndexedSeq[I],
                                                                                                              factory: F,
                                                                                                              offSpringSize: Int,
                                                                                                              selection: Selection[I],
                                                                                                              mutationOperator: Mutation[G, F],
                                                                                                              crossoverOperator: CrossOver[G, F]
                                                                                                               )(implicit aprng: Random): IndexedSeq[G] = {

    def breed(acc: List[G] = List.empty): List[G] = {
      if (acc.size >= offSpringSize) acc
      else {
        val newIndividuals = crossoverOperator(selection(archive).genome, selection(archive).genome, factory)
        breed(acc ++ newIndividuals)
      }
    }

    breed().toIndexedSeq
  }
}

abstract class NSGAII[G <: GAGenome, F <: GAGenomeFactory[G]](_maxStep: Int, archiveSize: Int, _factory: F, _evaluator: G => GAFitness)
  extends AbstractAlgorithm[G, F] {

  type I = Individual[G, GAFitness] with Distance with Ranking

  def evaluator = _evaluator

  def maxStep = _maxStep

  def factory = _factory

  def dominance: Dominant

  def rank: Rank

  def termination: AbstractTermination[I] = new SameRankingTermination[I](dominance, rank)

  def selection: Selection[I] = new BinaryTournamentNSGA2[I]

  def envSelection = new NonDominatedSorting(archiveSize)

  override def evolve(population: IndexedSeq[I])(implicit aprng: Random): IndexedSeq[I] = {

    val offspring = breed(
      population,
      population.size
    ).map {
      g => Individual(g, evaluator)
    }

    val archive = population ++ offspring
    //Elitisme strategy
    NSGAII.elitism(archive, envSelection, rank)(dominance)

  }

  def breed(archive: IndexedSeq[I],
            offSpringSize: Int)(implicit aprng: Random): IndexedSeq[G] = {

    NSGAII.breed[G, F, I](archive, factory, offSpringSize, selection, mutationOperator, crossoverOperator)
  }


}
