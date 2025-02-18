package mgo.evolution.algorithm

import cats.implicits.*
import mgo.evolution.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.evolution.ranking.*
import mgo.tools.ImplementEqualMethod
import mgo.tools.execution.*
import monocle.*
import monocle.syntax.all.*

import GenomeVectorDouble.*
import scala.reflect.ClassTag

object OSE:
  import CDGenome._
  import DeterministicIndividual._
  import monocle._

  type StateType[P] = (Archive[Individual[P]], OSEOperation.ReachMap)
  type OSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P]: Lens[OSEState[P], Archive[Individual[P]]] = Focus[OSEState[P]](_.s._1)
  def reachMapLens[P]: Lens[OSEState[P], OSEOperation.ReachMap] = Focus[OSEState[P]](_.s._2)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P](
    lambda: Int,
    operatorExploration: Double,
    continuous: Vector[C],
    discrete: Vector[D],
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    fitness: P => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[OSEState[P], Individual[P], Genome] =
    OSEOperation.adaptiveBreeding[OSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      origin,
      buildGenome(discrete),
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      archiveLens[P].get,
      reachMapLens.get)

  def expression[P](fitness: (IArray[Double], IArray[Int]) => P, components: Vector[C], discrete: Vector[D]): (Genome, Long, Boolean) => Individual[P] =
    DeterministicIndividual.expression(fitness, components, discrete)

  def elitism[P](mu: Int, limit: Vector[Double], origin: (IArray[Double], IArray[Int]) => Vector[Int], components: Vector[C], discrete: Vector[D], fitness: P => Vector[Double]): Elitism[OSEState[P], Individual[P]] =
    OSEOperation.elitism[OSEState[P], Individual[P]](
      individualFitness(fitness),
      limit,
      i => scaledValues(components, discrete)(i.genome),
      origin,
      mu,
      archiveLens[P],
      reachMapLens)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P], archive: Boolean)

  def result[P](state: OSEState[P], population: Vector[Individual[P]], continuous: Vector[C], discrete: Vector[D], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] =

    def individualToResult(i: Individual[P], archive: Boolean) =
      Result(scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), (i.focus(_.genome) andThen discreteVectorValues(discrete)).get, DeterministicIndividual.individualFitness(fitness)(i), i, archive)

    val goodIndividuals =
      if keepAll
      then population.map(i => individualToResult(i, false))
      else Seq()

    val archiveIndividuals =
      archiveLens.get(state).toVector.map(i => individualToResult(i, true))

    archiveIndividuals ++ goodIndividuals


  def result(t: OSE, state: OSEState[Vector[Double]], population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] =
    result[Vector[Double]](state = state, continuous = t.continuous, discrete = t.discrete, fitness = identity, population = population, keepAll = false)

  def reject(t: OSE): Option[Genome => Boolean] = NSGA2.reject(t.reject, t.continuous, t.discrete)

  given isAlgorithm: Algorithm[OSE, Individual[Vector[Double]], Genome, OSEState[Vector[Double]]] with
    override def initialState(t: OSE, rng: scala.util.Random) = EvolutionState(s = (Archive.empty, Array.empty))

    override def initialPopulation(t: OSE, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        OSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        OSE.expression(t.fitness, t.continuous, t.discrete),
        parallel)

    def step(t: OSE) =
      deterministic.step[OSEState[Vector[Double]], Individual[Vector[Double]], Genome](
        OSE.adaptiveBreeding[Vector[Double]](t.lambda, t.operatorExploration, t.continuous, t.discrete, t.origin, identity, reject(t)),
        OSE.expression(t.fitness, t.continuous, t.discrete),
        OSE.elitism(t.mu, t.limit, t.origin, t.continuous, t.discrete, identity),
        Focus[OSEState[Vector[Double]]](_.generation),
        Focus[OSEState[Vector[Double]]](_.evaluated))
  


case class OSE(
  mu: Int,
  lambda: Int,
  fitness: (IArray[Double], IArray[Int]) => Vector[Double],
  limit: Vector[Double],
  origin: (IArray[Double], IArray[Int]) => Vector[Int],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object OSEOperation:

  type ReachMap = Array[Vector[Int]]

  def filterAlreadyReached[G](origin: G => Vector[Int], reachMap: Set[Vector[Int]])(genomes: Vector[G]): Vector[G] =
    def keepNonReaching(g: G): Option[G] =
      reachMap.contains(origin(g)) match
        case true => None
        case false => Some(g)

    genomes.flatMap(g => keepNonReaching(g))

  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    archive: S => Archive[I],
    reachMap: S => ReachMap): Breeding[S, I, G] =
    (s, population, rng) =>
      val archivedPopulation = archive(s)
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, fitness, rng)
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

      val reached = reachMap(s).toSet

      val breeding: Breeding[S, I, G] =
        (s, pop, rng) =>
          val newGs =
            applyDynamicOperators(
              tournament(allRanks, tournamentRounds),
              genome andThen continuousValues,
              genome andThen discreteValues,
              continuousOperatorStatistics,
              discreteOperatorStatistics,
              discrete,
              operatorExploration,
              buildGenome)(s, pop, rng)
          filterAlreadyReached[G](g => origin(continuousValues(g), discreteValues(g)), reached)(newGs)

      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      randomTake(offspring, lambda, rng)

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]): Boolean =
    (fitness zip limit) forall ((f, l) => f <= l)

  def elitism[S, I: ClassTag](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (IArray[Double], IArray[Int]),
    origin: (IArray[Double], IArray[Int]) => Vector[Int],
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    reachMap: monocle.Lens[S, ReachMap]): Elitism[S, I] =
    (s, population, candidates, rng) =>

      val memoizedFitness = mgo.tools.memoize(fitness)
      val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), memoizedFitness)

      def o(i: I) = Function.tupled(origin)(values(i))
      val reached = reachMap.get(s).toSet

      def newlyReaching =
        def keepNewlyReaching(i: I): Option[I] =
          if patternIsReached(memoizedFitness(i), limit)
          then
            reached.contains(o(i)) match
              case true => None
              case false => Some(i)
          else None

        cloneRemoved.flatMap(i => keepNewlyReaching(i).toVector)

      val reaching = newlyReaching
      val s2 = reachMap.modify(_ ++ reaching.map(o)).compose(archive.modify(_ ++ reaching))(s)
      val filteredPopulation = filterAlreadyReached[I](i => Function.tupled(origin)(values(i)), reachMap.get(s2).toSet)(cloneRemoved)
      NSGA2Operations.elitism[S, I](memoizedFitness, values, mu)(s2, filteredPopulation, Vector.empty, rng)

