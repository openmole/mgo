package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.algorithm.OSEOperation.ReachMap
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.execution._
import monocle.function

import scala.reflect.ClassTag

object NoisyOSE {
  import CDGenome._
  import NoisyIndividual._

  type StateType[P] = (Archive[Individual[P]], OSEOperation.ReachMap)
  type OSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P] = EvolutionState.s[StateType[P]] composeLens function.fields.first
  def reachMapLens[P] = EvolutionState.s[StateType[P]] composeLens function.fields.second

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], rng: scala.util.Random) =
    CDGenome.initialGenomes(lambda, continuous, discrete, rng)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    discrete: Vector[D],
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    limit: Vector[Double],
    filter: Option[Genome => Boolean]) =
    NoisyOSEOperations.adaptiveBreeding[OSEState[P], Individual[P], Genome, P](
      vectorPhenotype[P].get,
      aggregation,
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      logOfPopulationSize,
      lambda,
      filter,
      operatorExploration,
      cloneProbability,
      origin,
      limit,
      archiveLens.get,
      reachMapLens.get)

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](fitness, continuous)

  def elitism[P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], origin: (Vector[Double], Vector[Int]) => Vector[Int], limit: Vector[Double]): Elitism[OSEState[P], Individual[P]] = {
    def individualValues(i: Individual[P]) = values(Individual.genome.get(i), components)

    NoisyOSEOperations.elitism[OSEState[P], Individual[P], P](
      vectorPhenotype[P].get,
      aggregation,
      individualValues,
      origin,
      limit,
      historySize,
      mergeHistories(individualValues, vectorPhenotype[P], Individual.historyAge[P], historySize),
      mu,
      archiveLens,
      reachMapLens)
  }

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result[P: Manifest](state: OSEState[P], population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], limit: Vector[Double]) = {
    def goodIndividuals =
      population.flatMap { i =>
        val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous)
        if (OSEOperation.patternIsReached(f, limit)) Some(Result(c, d, f, r)) else None
      }

    state.s._1.toVector.map { i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
      Result(c, d, f, r)
    } ++ goodIndividuals
  }

  def result[P: Manifest](noisyOSE: NoisyOSE[P], state: OSEState[P], population: Vector[Individual[P]]): Vector[Result] =
    result[P](state, population, noisyOSE.aggregation, noisyOSE.continuous, noisyOSE.limit)

  def filter[P](pse: NoisyOSE[P]) = NSGA2.filter(pse.filter, pse.continuous)

  implicit def isAlgorithm[P: Manifest]: Algorithm[NoisyOSE[P], Individual[P], Genome, OSEState[P]] = new Algorithm[NoisyOSE[P], Individual[P], Genome, OSEState[P]] {
    def initialState(t: NoisyOSE[P], rng: scala.util.Random) = EvolutionState(s = (Array.empty, Array.empty))

    def initialPopulation(t: NoisyOSE[P], rng: scala.util.Random) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyOSE.initialGenomes(t.lambda, t.continuous, t.discrete, rng),
        NoisyOSE.expression[P](t.fitness, t.continuous),
        rng)

    def step(t: NoisyOSE[P]) =
      noisy.step[OSEState[P], Individual[P], Genome](
        NoisyOSE.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          t.origin,
          t.limit,
          filter(t)),
        NoisyOSE.expression(t.fitness, t.continuous),
        NoisyOSE.elitism[P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.origin,
          t.limit),
        EvolutionState.generation)

  }

}

case class NoisyOSE[P](
  mu: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => P,
  limit: Vector[Double],
  origin: (Vector[Double], Vector[Int]) => Vector[Int],
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  filter: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object NoisyOSEOperations {

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def promisingReachMap[I](fitness: I => Vector[Double], limit: Vector[Double], origin: I => Vector[Int], population: Vector[I]): Set[Vector[Int]] = {
    val promising = population.filter(i => OSEOperation.patternIsReached(fitness(i), limit))
    promising.map(origin).toSet
  }

  def adaptiveBreeding[S, I, G, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    filter: Option[G => Boolean],
    operatorExploration: Double,
    cloneProbability: Double,
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    limit: Vector[Double],
    archive: S => Archive[I],
    reachMap: S => OSEOperation.ReachMap): Breeding[S, I, G] =
    (s, population, rng) => {

      def genomeOrigin(g: G) = origin(continuousValues(g), discreteValues(g))

      val promisingReachMapValue =
        promisingReachMap[I](
          aggregated(history, aggregation),
          limit,
          i => genomeOrigin(genome(i)),
          population)

      val reached = reachMap(s).toSet

      def filterAlreadyReachedAndNeighboursOfPromising(genomes: Vector[G]) =
        OSEOperation.filterAlreadyReached[G](genomeOrigin, reached)(genomes).filter(g => !promisingReachMapValue.contains(genomeOrigin(g)))

      val archivedPopulation = archive(s)
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, aggregated(history, aggregation), rng)
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

      def breeding: Breeding[S, I, G] =
        (s, pop, g) => {
          val breed = applyDynamicOperators[S, I, G](
            tournament(allRanks, tournamentRounds),
            genome andThen continuousValues,
            genome andThen discreteValues,
            continuousOperatorStatistics,
            discreteOperatorStatistics,
            discrete,
            operatorExploration,
            buildGenome)(s, pop, rng) //apply ()
          filterAlreadyReachedAndNeighboursOfPromising(breed)
        }

      val offspring = breed(breeding, lambda, filter)(s, population ++ archivedPopulation, rng)
      val sizedOffspringGenomes = randomTake[G](offspring, lambda, rng)
      clonesReplace(cloneProbability, population, genome, tournament(ranks, tournamentRounds))(s, sizedOffspringGenomes, rng)
    }

  def elitism[S, I: ClassTag, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    limit: Vector[Double],
    historySize: Int,
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    reachMap: monocle.Lens[S, ReachMap]): Elitism[S, I] =
    (s, population, candidates, rng) => {
      val merged = filterNaN(mergeHistories(population, candidates), aggregated(history, aggregation))
      val reached = reachMap.get(s).toSet

      def individualOrigin(i: I) = Function.tupled(origin)(values(i))
      def newlyReaching = {
        def keepNewlyReaching(i: I): Option[I] =
          if (OSEOperation.patternIsReached(aggregated(history, aggregation)(i), limit))
            (reached.contains(individualOrigin(i))) match {
              case true => None
              case false if history(i).size >= historySize => Some(i)
              case _ => None
            }
          else None

        merged.flatMap(i => keepNewlyReaching(i).toVector)
      }

      val reaching = newlyReaching
      val s2 = reachMap.modify(_ ++ reaching.map(individualOrigin)).compose(archive.modify(_ ++ reaching))(s)
      val filteredPopulation = OSEOperation.filterAlreadyReached[I](i => Function.tupled(origin)(values(i)), reachMap.get(s2).toSet)(merged)
      NoisyNSGA2Operations.elitism[S, I, P](aggregated(history, aggregation), values, mergeHistories, mu)(s2, filteredPopulation, Vector.empty, rng)
    }
}
