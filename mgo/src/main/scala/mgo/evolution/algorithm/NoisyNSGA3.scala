package mgo.evolution.algorithm

//import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._

import monocle._
import monocle.syntax.all._

object NoisyNSGA3 {

  import CDGenome._
  import NoisyIndividual._

  type NSGA3State = EvolutionState[Unit]

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int, individual: Individual[P])

  def result[P: Manifest](population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], discrete: Vector[D], keepAll: Boolean): Vector[Result[P]] = {
    val individuals = if (keepAll) population else keepFirstFront(population, fitness(aggregation))

    individuals.map: i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous, discrete)
      Result(c, d, f, r, i)

  }

  def result[P: Manifest](nsga3: NoisyNSGA3[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result[P](population, nsga3.aggregation, nsga3.continuous, nsga3.discrete, keepAll = false)

  def fitness[P: Manifest](aggregation: Vector[P] => Vector[Double]): Individual[P] => Vector[Double] =
    NoisyNSGA3Operations.aggregated[Individual[P], P](
      vectorPhenotype[P].get,
      aggregation,
      i => i.focus(_.phenotypeHistory).get.length.toDouble)(_)

  def initialGenomes(populationSize: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(populationSize, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P: Manifest](
    operatorExploration: Double,
    cloneProbability: Double,
    continuous: Vector[C],
    discrete: Vector[D],
    aggregation: Vector[P] => Vector[Double],
    reject: Option[Genome => Boolean],
    lambda: Int = -1): Breeding[S, Individual[P], Genome] =
    NoisyNSGA3Operations.adaptiveBreeding[S, Individual[P], Genome, P](
      fitness(aggregation),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      buildGenome(discrete),
      reject,
      operatorExploration,
      cloneProbability)

  def expression[P: Manifest](phenotype: (util.Random, IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    NoisyIndividual.expression[P](phenotype, continuous, discrete)

  def elitism[S, P: Manifest](mu: Int, references: NSGA3Operations.ReferencePoints, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], discrete: Vector[D]): Elitism[S, Individual[P]] = {
    def individualValues(i: Individual[P]) = scaledValues(components, discrete)(i.genome)

    NoisyNSGA3Operations.elitism[S, Individual[P]](
      fitness[P](aggregation),
      individualValues,
      mergeHistories(individualValues, vectorPhenotype[P], Focus[Individual[P]](_.historyAge), historySize),
      mu,
      references)
  }

  def reject[P](t: NoisyNSGA3[P]): Option[Genome => Boolean] = NSGA3.reject(t.reject, t.continuous, t.discrete)

  implicit def isAlgorithm[P: Manifest]: Algorithm[NoisyNSGA3[P], Individual[P], Genome, NSGA3State] =
    new Algorithm[NoisyNSGA3[P], Individual[P], Genome, NSGA3State] {
      override def initialState(t: NoisyNSGA3[P], rng: scala.util.Random) = EvolutionState(s = ())

      override def initialPopulation(t: NoisyNSGA3[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext): Vector[Individual[P]] =
        noisy.initialPopulation[Genome, Individual[P]](
          NoisyNSGA3.initialGenomes(t.popSize, t.continuous, t.discrete, reject(t), rng),
          NoisyNSGA3.expression[P](t.fitness, t.continuous, t.discrete),
          rng,
          parallel)

      override def step(t: NoisyNSGA3[P]) =
        noisy.step[NSGA3State, Individual[P], Genome](
          NoisyNSGA3.adaptiveBreeding[NSGA3State, P](t.operatorExploration, t.cloneProbability, t.continuous, t.discrete, t.aggregation, reject(t)),
          NoisyNSGA3.expression(t.fitness, t.continuous, t.discrete),
          NoisyNSGA3.elitism[NSGA3State, P](
            t.popSize, t.referencePoints,
            t.historySize,
            t.aggregation,
            t.continuous,
            t.discrete),
          Focus[NSGA3State](_.generation),
          Focus[NSGA3State](_.evaluated))

    }

}

case class NoisyNSGA3[P](
  popSize: Int,
  referencePoints: NSGA3Operations.ReferencePoints,
  fitness: (util.Random, IArray[Double], IArray[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object NoisyNSGA3Operations {

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double], accuracy: I => Double)(i: I): Vector[Double] = {
    aggregation(fitness(i)) ++ Vector(1.0 / accuracy(i))
  }

  def adaptiveBreeding[S, I, G, P](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    cloneProbability: Double,
    lambda: Int = -1): Breeding[S, I, G] = (s, population, rng) =>
    // same as deterministic, but eventually adding clones
    val breededGenomes = NSGA3Operations.adaptiveBreeding(fitness, genome, continuousValues, continuousOperator, discreteValues, discreteOperator, discrete, buildGenome, reject, operatorExploration, lambda)(s, population, rng)
    clonesReplace(cloneProbability, population, genome, randomSelection[S, I])(s, breededGenomes, rng)

  def elitism[S, I](
    fitness: I => Vector[Double],
    values: I => (IArray[Double], IArray[Int]),
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int,
    references: NSGA3Operations.ReferencePoints): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val mergedHistories = mergeHistories(population, candidates)
      val filtered: Vector[I] = filterNaN(mergedHistories, fitness)
      (s, NSGA3Operations.eliteWithReference[S, I](filtered, fitness, references, mu)(rng))

}

