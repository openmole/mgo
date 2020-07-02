package mgo.evolution.algorithm

//import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.tools.execution._

object NoisyNSGA3 {

  import CDGenome._
  import NoisyIndividual._

  type NSGA3State = EvolutionState[Unit]

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result[P: Manifest](population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C]): Vector[NoisyNSGA3.Result] =
    keepFirstFront(population, fitness(aggregation)).map {
      i =>
        val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
        Result(c, d, f, r)
    }

  def result[P: Manifest](nsga3: NoisyNSGA3[P], population: Vector[Individual[P]]): Vector[Result] =
    result[P](population, nsga3.aggregation, nsga3.continuous)

  def fitness[P: Manifest](aggregation: Vector[P] => Vector[Double]): Individual[P] => Vector[Double] =
    NoisyNSGA3Operations.aggregated[Individual[P], P](
      vectorPhenotype[P].get,
      aggregation,
      i => Individual.phenotypeHistory[P].get(i).length.toDouble)(_)

  def initialGenomes(populationSize: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(populationSize, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P: Manifest](
    operatorExploration: Double,
    cloneProbability: Double,
    discrete: Vector[D],
    aggregation: Vector[P] => Vector[Double],
    reject: Option[Genome => Boolean]): Breeding[S, Individual[P], Genome] =
    NoisyNSGA3Operations.adaptiveBreeding[S, Individual[P], Genome, P](
      fitness(aggregation),
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      reject,
      operatorExploration,
      cloneProbability)

  def expression[P: Manifest](phenotype: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](phenotype, continuous)

  def elitism[S, P: Manifest](mu: Int, references: NSGA3Operations.ReferencePoints, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C]): Elitism[S, Individual[P]] = {
    def individualValues(i: Individual[P]) = values(Individual.genome.get(i), components)

    NoisyNSGA3Operations.elitism[S, Individual[P]](
      fitness[P](aggregation),
      individualValues,
      mergeHistories(individualValues, vectorPhenotype[P], Individual.historyAge[P], historySize),
      mu,
      references)
  }

  def reject[P](pse: NoisyNSGA3[P]): Option[Genome => Boolean] = NSGA3.reject(pse.reject, pse.continuous)

  implicit def isAlgorithm[P: Manifest]: Algorithm[NoisyNSGA3[P], Individual[P], Genome, NSGA3State] =
    new Algorithm[NoisyNSGA3[P], Individual[P], Genome, NSGA3State] {
      override def initialState(t: NoisyNSGA3[P], rng: scala.util.Random) = EvolutionState(s = ())

      override def initialPopulation(t: NoisyNSGA3[P], rng: scala.util.Random): Vector[Individual[P]] =
        noisy.initialPopulation[Genome, Individual[P]](
          NoisyNSGA3.initialGenomes(t.popSize, t.continuous, t.discrete, reject(t), rng),
          NoisyNSGA3.expression[P](t.fitness, t.continuous),
          rng)

      override def step(t: NoisyNSGA3[P]): (NSGA3State, Vector[Individual[P]], scala.util.Random) => (NSGA3State, Vector[Individual[P]]) =
        (s, population, rng) =>
          noisy.step[NSGA3State, Individual[P], Genome](
            NoisyNSGA3.adaptiveBreeding[NSGA3State, P](t.operatorExploration, t.cloneProbability, t.discrete, t.aggregation, reject(t)),
            NoisyNSGA3.expression(t.fitness, t.continuous),
            NoisyNSGA3.elitism[NSGA3State, P](
              t.popSize, t.referencePoints,
              t.historySize,
              t.aggregation,
              t.continuous),
            EvolutionState.generation)(s, population, rng)

    }

}

case class NoisyNSGA3[P](
  popSize: Int,
  referencePoints: NSGA3Operations.ReferencePoints,
  fitness: (util.Random, Vector[Double], Vector[Int]) => P,
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object NoisyNSGA3Operations {

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double], accuracy: I => Double)(i: I): Vector[Double] = {
    aggregation(fitness(i)) ++ Vector(1.0 / accuracy(i))
  }

  def adaptiveBreeding[S, I, G, P](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    cloneProbability: Double): Breeding[S, I, G] = (s, population, rng) => {
    // same as deterministic, but eventually adding clones
    val breededGenomes = NSGA3Operations.adaptiveBreeding(fitness, genome, continuousValues, continuousOperator, discreteValues, discreteOperator, discrete, buildGenome, reject, operatorExploration)(s, population, rng)
    clonesReplace(cloneProbability, population, genome, randomSelection[S, I])(s, breededGenomes, rng)
  }

  def elitism[S, I](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    mergeHistories: (Vector[I], Vector[I]) => Vector[I],
    mu: Int,
    references: NSGA3Operations.ReferencePoints): Elitism[S, I] =
    (s, population, candidates, rng) => {
      val mergedHistories = mergeHistories(population, candidates)
      val filtered: Vector[I] = filterNaN(mergedHistories, fitness)
      (s, NSGA3Operations.eliteWithReference[S, I](filtered, fitness, references, mu)(rng))
    }

}

