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

package fr.iscpif

import mgo.genome
import mgo.genome.{ Sigma, GAGenomeWithSigma, GAGenome, GA }
import sun.net.www.content.text.plain

package object mgo {
  implicit def traversable2Population[G, P, F, I](seq: Traversable[PopulationElement[G, P, F, I]]) =
    new Population[G, P, F, I] {
      override val content = seq.toIndexedSeq
    }

  implicit def population2IndexedSeq[G, P, F, I](pop: Population[G, P, F, I]) = pop.content

  private def changeScale(v: Double, min: Double, max: Double, boundaryMin: Double, boundaryMax: Double) = {
    val factor = (boundaryMax - boundaryMin) / (max - min)
    (factor * (v - min) + boundaryMin)
  }

  implicit def double2Scalable(d: Double) = new {
    def scale(min: Double, max: Double) = changeScale(d, 0, 1, min, max)
    def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  implicit class StateIteratorDecorator[S <: { def terminated: Boolean }](i: Iterator[S]) {
    def untilConverged(f: S => Unit) = i.drop(1).dropWhile { s => f(s); !s.terminated }.next
  }

  type Map = algorithm.Map
  type Novelty = algorithm.Novelty
  type NSGAII = algorithm.NSGAII
  type Profile = algorithm.Profile
  type SMSEMOEA = algorithm.SMSEMOEA
  type A = archive.A
  type Archive = archive.Archive
  type NoArchive = archive.NoArchive
  type NoveltyArchive = archive.NoveltyArchive
  type Breeding = breed.Breeding
  type GeneticBreeding = breed.GeneticBreeding
  type AverageCrossOver = crossover.AverageCrossOver
  type CrossOver = crossover.CrossOver
  type IdentityCrossOver = crossover.IdentityCrossOver
  type SBXBoundedCrossover = crossover.SBXBoundedCrossOver
  type UniformCrossOver = crossover.UniformCrossOver
  type CrowdingDiversity = diversity.CrowdingDiversity
  type Diversity = diversity.Diversity
  type DiversityMetric = diversity.DiversityMetric
  type HypervolumeDiversity = diversity.HypervolumeDiversity
  type NoDiversity = diversity.NoDiversity
  type Dominance = dominance.Dominance
  type NonStrictDominance = dominance.NonStrictDominance
  type NonStrictEpsilonDominance = dominance.NonStrictEpsilonDominance
  type StrictDominance = dominance.StrictDominance
  type StrictEpsilonDominance = dominance.StrictEpsilonDominance
  type Elitism = elitism.Elitism
  type MapElitism = elitism.MapElitism
  type NonDominatedElitism = elitism.NonDominatedElitism
  type ProfileElitism = elitism.ProfileElitism
  type Aggregation = fitness.Aggregation
  type F = fitness.F
  type Fitness = fitness.Fitness
  type MaxAggregation = fitness.MaxAggregation
  type MG = fitness.MG
  type MGFitness = fitness.MGFitness
  val MGFitness = fitness.MGFitness
  type G = genome.G
  type GAGenome = genome.GAGenome
  type GAGenomeWithSigma = genome.GAGenomeWithSigma
  type GA = genome.GA
  type Sigma = genome.Sigma
  type MapGenomePlotter = map.MapGenomePlotter
  type MapPlotter = map.MapPlotter
  type ProfileGenomePlotter = map.ProfileGenomePlotter
  type ProfilePlotter = map.ProfilePlotter
  val Hypervolume = metric.Hypervolume
  type ReferencePoint = metric.ReferencePoint
  type CloneRemoval = modifier.CloneRemoval
  type CrowdingGenomicDiversity = modifier.CrowdingGenomicDiversity
  type DiversityModifier = modifier.DiversityModifier
  type EuclideanGenomicDiversity = modifier.EuclideanGenomicDiversity
  type IndividualFilter = modifier.IndividualFilter
  type Modifier = modifier.Modifier
  type NoModifier = modifier.NoModifier
  type NoveltyModifier = modifier.NoveltyModifier
  type ProfileModifier = modifier.ProfileModifier
  type RankModifier = modifier.RankModifier
  type IndividualDiversityModifier = modifier.IndividualDiversityModifier
  type RankDiversityModifier = modifier.RankDiversityModifier
  type CoEvolvingSigmaValuesMutation = mutation.CoEvolvingSigmaValuesMutation
  type GaussianMutation = mutation.GaussianMutation
  type Mutation = mutation.Mutation
  type IdentityMutation = mutation.IdentityMutation
  type RandomMutation = mutation.RandomMutation
  type P = phenotype.P
  type Evaluation = phenotype.Evaluation
  type Problem = problem.Problem
  type GAProblem = problem.GAProblem
  type GAGenomePhenotype = problem.GAGenomePhenotype
  type Scaling = problem.Scaling
  type HierarchicalRanking = ranking.HierarchicalRanking
  type NoRanking = ranking.NoRanking
  type ParetoRanking = ranking.ParetoRanking
  type Rank = ranking.Rank
  type Ranking = ranking.Ranking
  type BinaryTournamentSelection = selection.BinaryTournamentSelection
  type MapSelection = selection.MapSelection
  type Selection = selection.Selection
  type CrowdingStabilityTermination = termination.CrowdingStabilityTermination
  type CounterTermination = termination.CounterTermination
  type HyperVolumeStabilityTermination = termination.HyperVolumeStabilityTermination
  type StabilityTermination = termination.StabilityTermination
  type Termination = termination.Termination
  type TimedTermination = termination.TimedTermination

}