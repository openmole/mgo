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
import org.apache.commons.math3.random._

package object mgo {
  implicit def traversable2Population[G, P, F](elements: Seq[PopulationElement[G, P, F]]) = Population(elements)
  implicit def population2IndexedSeq[G, P, F, I](pop: Population[G, P, F]) = pop.content

  private def changeScale(v: Double, min: Double, max: Double, boundaryMin: Double, boundaryMax: Double) = {
    val factor = (boundaryMax - boundaryMin) / (max - min)
    (factor * (v - min) + boundaryMin)
  }

  implicit def double2Scalable(d: Double) = new {
    def scale(min: Double, max: Double) = changeScale(d, 0, 1, min, max)
    def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  implicit class StateIteratorDecorator[S <: { def terminated: Boolean }](i: Iterator[S]) {
    def last = i.drop(1).dropWhile { !_.terminated }.next
    def untilConverged(f: S => Unit) = i.drop(1).dropWhile { s => f(s); !s.terminated }.next
  }

  //TODO: unused rng and newRNG functions?
  object rng {
    implicit def rng = newRNG
  }

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))
  def newRNG = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a)))

  type AggregatedOptimisation = algorithm.AggregatedOptimisation
  type PSE = algorithm.PSE
  type CMAES = algorithm.CMAES
  type Map = algorithm.Map
  type NSGAII = algorithm.NSGAII
  type Profile = algorithm.Profile
  type SMSEMOEA = algorithm.SMSEMOEA
  type A = archive.A
  type Archive = archive.Archive
  type ArchiveIndividuals = archive.ArchiveIndividuals
  type CMAESArchive = archive.CMAESArchive
  type HitMapArchive = archive.HitMapArchive
  type NEATArchive = archive.NEATArchive
  type NoArchive = archive.NoArchive
  type NoveltyArchive = archive.NoveltyArchive
  type Breeding = breed.Breeding
  type BreedingContext = breed.BreedingContext
  type BreedingContextId = breed.BreedingContextId
  type CMAESBreeding = breed.CMAESBreeding
  type GeneticBreeding = breed.GeneticBreeding
  type Cloning = breed.Cloning
  type CloningPure = breed.CloningPure
  lazy val BLXCrossover = crossover.BLXCrossover
  type Crossover = crossover.Crossover
  type DynamicGACrossover = crossover.DynamicGACrossover
  lazy val IdentityCrossOver = crossover.IdentityCrossover
  lazy val SBXCrossover = crossover.SBXCrossover
  type CrowdingIndividualDistance = distance.CrowdingIndividualDistance
  type ClosedCrowdingIndividualDistance = distance.ClosedCrowdingIndividualDistance
  type ClosedCrowdingIndividualDistanceFromArchive = distance.ClosedCrowdingIndividualDistanceFromArchive
  type EuclideanIndividualDiversity = distance.EuclideanIndividualDiversity
  type IndividualPosition = distance.IndividualPosition
  type IndividualDistance = distance.IndividualDistance
  type PhenotypeIsPosition = distance.PhenotypeIsPosition
  type IndividualDistanceFromArchive = distance.IndividualDistanceFromArchive
  type KNearestNeighboursIndividualDistance = distance.KNearestNeighboursIndividualDistance
  type FitnessCrowdingDiversity = diversity.FitnessCrowdingDiversity
  type FitnessClosedCrowdingDiversity = diversity.FitnessClosedCrowdingDiversity
  type KNearestNeighboursDiversity = diversity.FitnessKNearestNeighboursDiversity
  type Diversity = diversity.Diversity
  type FitnessHypervolumeDiversity = diversity.FitnessHypervolumeDiversity
  type NoDiversity = diversity.NoDiversity
  type Dominance = dominance.Dominance
  type NonStrictDominance = dominance.NonStrictDominance
  type NonStrictEpsilonDominance = dominance.NonStrictEpsilonDominance
  type StrictDominance = dominance.StrictDominance
  type StrictEpsilonDominance = dominance.StrictEpsilonDominance
  type BestAggregatedElitism = elitism.BestAggregatedElitism
  type BestAggregatedNicheElitism = elitism.BestAggregatedNicheElitism
  type BestRankedNicheElitism = elitism.BestRankedNicheElitism
  type CloneRemoval = elitism.CloneRemoval
  type ConservativeFIFOAggregatedElitism = elitism.ConservativeFIFOAggregatedElitism
  type DiversityAggregatedElitism = elitism.DiversityAggregatedElitism
  type Elitism = elitism.Elitism
  type NaNRemoval = elitism.NaNRemoval
  type NEATElitism = elitism.NEATElitism
  type KeepOffspringElitism = elitism.KeepOffspringElitism
  type IndividualFilter = elitism.IndividualFilter
  type NonDominatedElitism = elitism.NonDominatedElitism
  type NicheElitism = elitism.NicheElitism
  type RandomNicheElitism = elitism.RandomNicheElitism
  type RankElitism = elitism.RankElitism
  type Aggregation = fitness.Aggregation
  type F = fitness.F
  type DoubleFitness = fitness.DoubleFitness
  type MaxAggregation = fitness.MaxAggregation
  type MG = fitness.MG
  type MGFitness = fitness.MGFitness
  type ClampedGenome = genome.ClampedGenome
  type G = genome.G
  type GAGenome = genome.GAGenome
  type GAGenomeWithRandomValue = genome.GAGenomeWithRandomValue
  type GAGenomeWithSigma = genome.GAGenomeWithSigma
  type NEATGenome = genome.NEATGenome
  type GA = genome.GA
  type NoGenomeClamping = genome.NoGenomeClamping
  type RandomValue = genome.RandomValue
  type RandomInitialGenome = genome.RandomInitialGenome
  type Sigma = genome.Sigma
  type MapGenomePlotter = map.MapGenomePlotter
  type MapPlotter = map.MapPlotter
  type ProfileGenomePlotter = map.ProfileGenomePlotter
  type ProfilePlotter = map.ProfilePlotter
  lazy val AdaptiveCauchyMutation = mutation.AdaptiveCauchyMutation
  lazy val BGAMutation = mutation.BGAMutation
  type DynamicGAMutation = mutation.DynamicGAMutation
  lazy val GaussianMutation = mutation.GaussianMutation
  type Mutation = mutation.Mutation
  lazy val IdentityMutation = mutation.IdentityMutation
  type GAGenotypeGridNiche = niche.GAGenotypeGridNiche
  type MapNiche = niche.MapNiche
  type Niche = niche.Niche
  type PhenotypeGridNiche = niche.PhenotypeGridNiche
  type ProfileNiche = niche.ProfileNiche
  type P = phenotype.P
  type Evaluation = phenotype.Evaluation
  type DoubleSeqPhenotype = phenotype.DoubleSeqPhenotype
  type NoPhenotype = phenotype.NoPhenotype
  type Problem = problem.Problem
  type GAProblem = problem.GAProblem
  type NEATProblem = problem.NEATProblem
  type NoFitness = problem.NoFitness
  type Scaling = problem.Scaling
  type HierarchicalRanking = ranking.HierarchicalRanking
  type HypervolumeRanking = ranking.HypervolumeRanking
  type NoRanking = ranking.NoRanking
  type ParetoRanking = ranking.ParetoRanking
  type ProfileRanking = ranking.ProfileRanking
  type Ranking = ranking.Ranking
  type BinaryTournamentSelection = selection.BinaryTournamentSelection
  type Mating = selection.Mating
  type RandomSelection = selection.RandomSelection
  type RandomMating = selection.PairMating
  type TournamentOnDiversity = selection.TournamentOnDiversity
  type TournamentOnRank = selection.TournamentOnRank
  type TournamentOnRankAndDiversity = selection.TournamentOnRankAndDiversity
  type MapSelection = selection.MapSelection
  type ProportionalNumberOfRound = selection.ProportionalNumberOfRound
  type Selection = selection.Selection
  type TournamentOnAggregatedFitness = selection.TournamentOnAggregatedFitness
  type TournamentOnHitCount = selection.TournamentOnHitCount
  type ConditionalTermination = termination.ConditionalTermination
  type CrowdingStabilityTermination = termination.CrowdingStabilityTermination
  type CounterTermination = termination.CounterTermination
  type HyperVolumeStabilityTermination = termination.HyperVolumeStabilityTermination
  type StabilityTermination = termination.StabilityTermination
  type Termination = termination.Termination
  type TimedTermination = termination.TimedTermination

}
