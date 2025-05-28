package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.algorithm.OSEOperation.ReachMap
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.*

import monocle.function
import monocle._
import monocle.syntax.all._

import scala.reflect.ClassTag

object NoisyOSE {
  import CDGenome._
  import NoisyIndividual._

  type StateType[P] = (Archive[Individual[P]], OSEOperation.ReachMap)
  type OSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P]: Lens[EvolutionState[StateType[P]], Archive[Individual[P]]] = Focus[EvolutionState[StateType[P]]](_.s._1)
  def reachMapLens[P]: Lens[EvolutionState[StateType[P]], ReachMap] = Focus[EvolutionState[StateType[P]]](_.s._2)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    continuous: Vector[C],
    discrete: Vector[D],
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    limit: Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[OSEState[P], Individual[P], Genome] =
    NoisyOSEOperations.adaptiveBreeding[OSEState[P], Individual[P], Genome, P](
      vectorPhenotype[P].get,
      aggregation,
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      buildGenome(discrete),
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      cloneProbability,
      origin,
      limit,
      archiveLens.get,
      reachMapLens.get)

  def expression[P: Manifest](fitness: (util.Random, IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    NoisyIndividual.expression[P](fitness, continuous, discrete)

  def elitism[P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], discrete: Vector[D], origin: (IArray[Double], IArray[Int]) => Vector[Int], limit: Vector[Double]): Elitism[OSEState[P], Individual[P]] =
    def individualValues(i: Individual[P]) = scaledValues(components, discrete)(i.genome)

    NoisyOSEOperations.elitism[OSEState[P], Individual[P], P](
      vectorPhenotype[P].get,
      aggregation,
      individualValues,
      origin,
      limit,
      historySize,
      mergeHistories(individualValues, vectorPhenotype[P], Focus[Individual[P]](_.historyAge), historySize),
      mu,
      archiveLens,
      reachMapLens)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int, individual: Individual[P], archive: Boolean)

  def result[P: Manifest](state: OSEState[P], population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], discrete: Vector[D], limit: Vector[Double], keepAll: Boolean): Vector[Result[P]] =
    def goodIndividuals =
      population.flatMap: i =>
        val (c, d, f, r) = NoisyIndividual.aggregate[P](i, aggregation, continuous, discrete)
        if (keepAll || OSEOperation.patternIsReached(f, limit)) then Some(Result(c, d, f, r, i, false)) else None

    state.s._1.toVector.map: i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous, discrete)
      Result(c, d, f, r, i, true)
    ++ goodIndividuals

  def result[P: Manifest](noisyOSE: NoisyOSE[P], state: OSEState[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result[P](state, population, noisyOSE.aggregation, noisyOSE.continuous, noisyOSE.discrete, noisyOSE.limit, keepAll = false)

  def reject[P](t: NoisyOSE[P]): Option[Genome => Boolean] = NSGA2.reject(t.reject, t.continuous, t.discrete)

  implicit def isAlgorithm[P: Manifest]: Algorithm[NoisyOSE[P], Individual[P], Genome, OSEState[P]] = new Algorithm[NoisyOSE[P], Individual[P], Genome, OSEState[P]] {
    def initialState(t: NoisyOSE[P], rng: scala.util.Random) = EvolutionState(s = (Archive.empty, Array.empty))

    def initialPopulation(t: NoisyOSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyOSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyOSE.expression[P](t.fitness, t.continuous, t.discrete),
        rng,
        parallel)

    def step(t: NoisyOSE[P]) =
      noisy.step[OSEState[P], Individual[P], Genome](
        NoisyOSE.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.continuous,
          t.discrete,
          t.origin,
          t.limit,
          reject(t)),
        NoisyOSE.expression(t.fitness, t.continuous, t.discrete),
        NoisyOSE.elitism[P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.discrete,
          t.origin,
          t.limit),
        Focus[OSEState[P]](_.generation),
        Focus[OSEState[P]](_.evaluated))

  }

}

case class NoisyOSE[P](
  mu: Int,
  lambda: Int,
  fitness: (util.Random, IArray[Double], IArray[Int]) => P,
  limit: Vector[Double],
  origin: (IArray[Double], IArray[Int]) => Vector[Int],
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

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
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    cloneProbability: Double,
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    limit: Vector[Double],
    archive: S => Archive[I],
    reachMap: S => OSEOperation.ReachMap): Breeding[S, I, G] =
    (s, population, rng) =>

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
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, aggregated(history, aggregation))
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      val genomeValue = genome andThen (continuousValues, discreteValues).tupled

      def breeding: Breeding[S, I, G] =
        (s, pop, g) => {
          val breed = applyDynamicOperators[S, I, G](
            tournament(allRanks, tournamentRounds),
            genomeValue,
            continuousOperatorStatistics,
            discreteOperatorStatistics,
            discrete,
            operatorExploration,
            buildGenome)(s, pop, rng) //apply ()
          filterAlreadyReachedAndNeighboursOfPromising(breed)
        }

      val offspring = breed(breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      clonesReplace(cloneProbability, population, genome, tournament(ranks, tournamentRounds))(s, offspring, rng)

  def elitism[S, I: ClassTag, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
    values: I => (IArray[Double], IArray[Int]),
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    limit: Vector[Double],
    historySize: Int,
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    reachMap: monocle.Lens[S, ReachMap]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val fitness = aggregated(history, aggregation).memoized
      val merged = filterNaN(mergeHistories(population, candidates), fitness)
      val reached = reachMap.get(s).toSet

      def individualOrigin(i: I) = Function.tupled(origin)(values(i))
      def newlyReaching =
        def keepNewlyReaching(i: I): Option[I] =
          if OSEOperation.patternIsReached(fitness(i), limit)
          then
            reached.contains(individualOrigin(i)) match
              case true => None
              case false if history(i).size >= historySize => Some(i)
              case _ => None

          else None

        merged.flatMap(i => keepNewlyReaching(i).toVector)

      val reaching = newlyReaching
      val s2 = reachMap.modify(_ ++ reaching.map(individualOrigin)).compose(archive.modify(_ ++ reaching))(s)
      val filteredPopulation = OSEOperation.filterAlreadyReached[I](i => Function.tupled(origin)(values(i)), reachMap.get(s2).toSet)(merged)
      NoisyNSGA2Operations.elitism[S, I, P](fitness, mergeHistories, mu)(s2, filteredPopulation, Vector.empty, rng)

}
