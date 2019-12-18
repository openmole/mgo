package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools.execution._

import scala.reflect.ClassTag

object OSE {
  import CDGenome._
  import DeterministicIndividual._
  import monocle._

  type StateType[P] = (Archive[Individual[P]], OSEOperation.ReachMap)
  type OSEState[P] = EvolutionState[StateType[P]]

  def archiveLens[P] = EvolutionState.s[StateType[P]] composeLens function.fields.first
  def reachMapLens[P] = EvolutionState.s[StateType[P]] composeLens function.fields.second

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], rng: scala.util.Random) =
    CDGenome.initialGenomes(lambda, continuous, discrete, rng)

  def adaptiveBreeding[P](
    lambda: Int,
    operatorExploration: Double,
    discrete: Vector[D],
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    fitness: P => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[OSEState[P], Individual[P], Genome] =
    OSEOperation.adaptiveBreeding[OSEState[P], Individual[P], Genome](
      individualFitness(fitness),
      Individual.genome.get,
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

  def expression[P](fitness: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(fitness, components)

  def elitism[P](mu: Int, limit: Vector[Double], origin: (Vector[Double], Vector[Int]) => Vector[Int], components: Vector[C], fitness: P => Vector[Double]) =
    OSEOperation.elitism[OSEState[P], Individual[P]](
      individualFitness(fitness),
      limit,
      i => values(Individual.genome.get(i), components),
      origin,
      mu,
      archiveLens[P],
      reachMapLens)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result[P](state: OSEState[P], continuous: Vector[C], fitness: P => Vector[Double]) =
    archiveLens.get(state).toVector.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, DeterministicIndividual.individualFitness(fitness)(i))
    }

  def result(ose: OSE, state: OSEState[Vector[Double]]): Vector[Result] =
    result[Vector[Double]](state = state, continuous = ose.continuous, fitness = identity)

  def reject(ose: OSE) = NSGA2.reject(ose.reject, ose.continuous)

  implicit def isAlgorithm: Algorithm[OSE, Individual[Vector[Double]], Genome, OSEState[Vector[Double]]] = new Algorithm[OSE, Individual[Vector[Double]], Genome, OSEState[Vector[Double]]] {
    override def initialState(t: OSE, rng: scala.util.Random) = EvolutionState(s = (Array.empty, Array.empty))

    override def initialPopulation(t: OSE, rng: scala.util.Random) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        OSE.initialGenomes(t.lambda, t.continuous, t.discrete, rng),
        OSE.expression(t.fitness, t.continuous))

    def step(t: OSE) =
      deterministic.step[OSEState[Vector[Double]], Individual[Vector[Double]], Genome](
        OSE.adaptiveBreeding[Vector[Double]](t.lambda, t.operatorExploration, t.discrete, t.origin, identity, reject(t)),
        OSE.expression(t.fitness, t.continuous),
        OSE.elitism(t.mu, t.limit, t.origin, t.continuous, identity),
        EvolutionState.generation)

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

  def filterAlreadyReached[G](origin: G => Vector[Int], reachMap: Set[Vector[Int]])(genomes: Vector[G]) = {
    def keepNonReaching(g: G): Option[G] =
      reachMap.contains(origin(g)) match {
        case true => None
        case false => Some(g)
      }

    genomes.flatMap(g => keepNonReaching(g))
  }

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

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]) =
    (fitness zip limit) forall { case (f, l) => f <= l }

  def elitism[S, I: ClassTag](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    mu: Int,
    archive: monocle.Lens[S, Archive[I]],
    reachMap: monocle.Lens[S, ReachMap]): Elitism[S, I] =
    (s, population, candidates, rng) => {

      val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), fitness)

      def o(i: I) = Function.tupled(origin)(values(i))

      val reached = reachMap.get(s).toSet

      def newlyReaching = {
        def keepNewlyReaching(i: I): Option[I] =
          if (patternIsReached(fitness(i), limit))
            reached.contains(o(i)) match {
              case true => None
              case false => Some(i)
            }
          else None

        cloneRemoved.flatMap(i => keepNewlyReaching(i).toVector)
      }

      val reaching = newlyReaching
      val s2 = reachMap.modify(_ ++ reaching.map(o)).compose(archive.modify(_ ++ reaching))(s)
      val filteredPopulation = filterAlreadyReached[I](i => Function.tupled(origin)(values(i)), reachMap.get(s2).toSet)(cloneRemoved)
      NSGA2Operations.elitism[S, I](fitness, values, mu)(s2, filteredPopulation, Vector.empty, rng)
    }

}
