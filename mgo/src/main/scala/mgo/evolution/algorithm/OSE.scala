package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.execution._

import monocle._
import monocle.syntax.all._

import scala.reflect.ClassTag

object OSE {
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
    discrete: Vector[D],
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    fitness: P => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[OSEState[P], Individual[P], Genome] =
    OSEOperation.adaptiveBreeding[OSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      origin,
      buildGenome,
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration,
      archiveLens[P].get,
      reachMapLens.get)

  def expression[P](fitness: (Vector[Double], Vector[Int]) => P, components: Vector[C]): (Genome, Long, Boolean) => Individual[P] =
    DeterministicIndividual.expression(fitness, components)

  def elitism[P](mu: Int, limit: Vector[Double], origin: (Vector[Double], Vector[Int]) => Vector[Int], components: Vector[C], fitness: P => Vector[Double]): Elitism[OSEState[P], Individual[P]] =
    OSEOperation.elitism[OSEState[P], Individual[P]](
      individualFitness(fitness),
      limit,
      i => values(i.genome, components),
      origin,
      mu,
      archiveLens[P],
      reachMapLens)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P])

  def result[P](state: OSEState[P], population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] = {
    val indivduals = archiveLens.get(state).toVector ++ { if (keepAll) population else Seq() }

    indivduals.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), i.focus(_.genome) andThen discreteValues get, DeterministicIndividual.individualFitness(fitness)(i), i)
    }
  }

  def result(ose: OSE, state: OSEState[Vector[Double]], population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] =
    result[Vector[Double]](state = state, continuous = ose.continuous, fitness = identity, population = population, keepAll = false)

  def reject(ose: OSE): Option[Genome => Boolean] = NSGA2.reject(ose.reject, ose.continuous)

  implicit def isAlgorithm: Algorithm[OSE, Individual[Vector[Double]], Genome, OSEState[Vector[Double]]] = new Algorithm[OSE, Individual[Vector[Double]], Genome, OSEState[Vector[Double]]] {
    override def initialState(t: OSE, rng: scala.util.Random) = EvolutionState(s = (Array.empty, Array.empty))

    override def initialPopulation(t: OSE, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        OSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        OSE.expression(t.fitness, t.continuous),
        parallel)

    def step(t: OSE) =
      deterministic.step[OSEState[Vector[Double]], Individual[Vector[Double]], Genome](
        OSE.adaptiveBreeding[Vector[Double]](t.lambda, t.operatorExploration, t.discrete, t.origin, identity, reject(t)),
        OSE.expression(t.fitness, t.continuous),
        OSE.elitism(t.mu, t.limit, t.origin, t.continuous, identity),
        Focus[OSEState[Vector[Double]]](_.generation),
        Focus[OSEState[Vector[Double]]](_.evaluated))

  }

}

case class OSE(
  mu: Int,
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  limit: Vector[Double],
  origin: (Vector[Double], Vector[Int]) => Vector[Int],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object OSEOperation {

  type ReachMap = Array[Vector[Int]]


  def filterAlreadyReached[G](origin: G => Vector[Int], reachMap: Set[Vector[Int]])(genomes: Vector[G]): Vector[G] =
    def keepNonReaching(g: G): Option[G] =
      reachMap.contains(origin(g)) match {
        case true => None
        case false => Some(g)
      }

    genomes.flatMap(g => keepNonReaching(g))

  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    archive: S => Archive[I],
    reachMap: S => ReachMap): Breeding[S, I, G] =
    (s, population, rng) => {
      val archivedPopulation = archive(s)
      val ranks = ranking.paretoRankingMinAndCrowdingDiversity[I](population, fitness, rng)
      val allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
      val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

      val reached = reachMap(s).toSet

      val breeding: Breeding[S, I, G] =
        (s, pop, rng) => {
          val newGs =
            applyDynamicOperators[S, I, G](
              tournament(allRanks, tournamentRounds),
              genome andThen continuousValues,
              genome andThen discreteValues,
              continuousOperatorStatistics,
              discreteOperatorStatistics,
              discrete,
              operatorExploration,
              buildGenome)(s, pop, rng)
          filterAlreadyReached[G](g => origin(continuousValues(g), discreteValues(g)), reached)(newGs)
        }

      val offspring = breed[S, I, G](breeding, lambda, reject)(s, population ++ archivedPopulation, rng)
      randomTake(offspring, lambda, rng)
    }

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]): Boolean =
    (fitness zip limit) forall { case (f, l) => f <= l }

  def elitism[S, I: ClassTag](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
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


}
