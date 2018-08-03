package mgo.algorithm

import freedsl.io.IOInterpreter
import freedsl.random.RandomInterpreter
import freedsl.system.SystemInterpreter
import freestyle.tagless.tagless
import mgo._
import mgo.ranking._
import mgo.breeding._
import mgo.elitism._
import mgo.contexts._
import mgo.tools._
import tools._
import freedsl.dsl._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freedsl.tool._
import shapeless._

object NoisyOSE {
  import CDGenome._
  import NoisyIndividual._

  type OSEState = (Array[Individual], Array[Vector[Int]])

  @tagless trait IndividualArchive {
    def put(i: Seq[Individual]): FS[Unit]
    def get(): FS[Vector[Individual]]
  }

  implicit def archiveConvert[M[_]](implicit vhm: IndividualArchive[M]) = new Archive[M, Individual] {
    def put(i: Seq[Individual]) = vhm.put(i)
    def get() = vhm.get()
  }

  case class ArchiveInterpreter(val archive: collection.mutable.Buffer[Individual]) extends IndividualArchive.Handler[Evaluated] {
    def put(i: Seq[Individual]) = freedsl.dsl.result(archive ++= i)
    def get() = freedsl.dsl.result(archive.toVector)
  }

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation: ReachMap](lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double], discrete: Vector[D], origin: (Vector[Double], Vector[Int]) => Vector[Int], limit: Vector[Double])(implicit archive: Archive[M, Individual]): Breeding[M, Individual, Genome] =
    NoisyOSEOperations.adaptiveBreeding[M, Individual, Genome](
      vectorFitness.get,
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
      operatorExploration,
      cloneProbability,
      origin,
      limit)

  def expression(fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    NoisyIndividual.expression(fitness, continuous)

  def elitism[M[_]: cats.Monad: Random: Generation: ReachMap](mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], components: Vector[C], origin: (Vector[Double], Vector[Int]) => Vector[Int], limit: Vector[Double])(implicit archive: Archive[M, Individual]): Elitism[M, Individual] =
    NoisyOSEOperations.elitism[M, Individual](
      vectorFitness.get,
      aggregation,
      i => values(Individual.genome.get(i), components),
      origin,
      limit,
      historySize,
      mergeHistories(Individual.historyAge, vectorFitness)(historySize),
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result(state: EvolutionState[OSEState], population: Vector[Individual], aggregation: Vector[Vector[Double]] => Vector[Double], continuous: Vector[C], limit: Vector[Double]) = {
    def goodIndividuals =
      population.flatMap { i =>
        val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
        if (OSEOperation.patternIsReached(f, limit)) Some(Result(c, d, f, r)) else None
      }

    state.s._1.toVector.map { i =>
      val (c, d, f, r) = NoisyIndividual.aggregate(i, aggregation, continuous)
      Result(c, d, f, r)
    } ++ goodIndividuals
  }

  def result(noisyOSE: NoisyOSE, state: EvolutionState[OSEState], population: Vector[Individual]): Vector[Result] =
    result(state, population, noisyOSE.aggregation, noisyOSE.continuous, noisyOSE.limit)

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit archive: Archive[M, Individual], reachMap: ReachMap[M]) = for {
    map <- reachMap.get()
    arch <- archive.get()
    s <- mgo.algorithm.state[M, OSEState]((arch.toArray, map.toArray))
  } yield s

  object OSEImplicits {
    def apply(state: EvolutionState[OSEState]): OSEImplicits =
      OSEImplicits()(
        GenerationInterpreter(state.generation),
        RandomInterpreter(state.random),
        StartTimeInterpreter(state.startTime),
        IOInterpreter(),
        ArchiveInterpreter(state.s._1.to[collection.mutable.Buffer]),
        ReachMapInterpreter(state.s._2.to[collection.mutable.HashSet]),
        SystemInterpreter())
  }

  case class OSEImplicits(implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, archiveInterpreter: ArchiveInterpreter, reachMapInterpreter: ReachMapInterpreter, systemInterpreter: SystemInterpreter)

  def run[T](rng: util.Random)(f: OSEImplicits => T): T = {
    val state = EvolutionState[OSEState](random = rng, s = (Array.empty, Array.empty))
    run(state)(f)
  }

  def run[T, S](state: EvolutionState[OSEState])(f: OSEImplicits => T): T = f(OSEImplicits(state))

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime: ReachMap](implicit archive: Archive[M, Individual]): Algorithm[NoisyOSE, M, Individual, Genome, EvolutionState[OSEState]] = new Algorithm[NoisyOSE, M, Individual, Genome, EvolutionState[OSEState]] {
    def initialPopulation(t: NoisyOSE) =
      noisy.initialPopulation[M, Genome, Individual](
        NoisyOSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyOSE.expression(t.fitness, t.continuous))

    def step(t: NoisyOSE): Kleisli[M, Vector[Individual], Vector[Individual]] =
      noisy.step[M, Individual, Genome](
        NoisyOSE.adaptiveBreeding[M](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          t.origin,
          t.limit),
        NoisyOSE.expression(t.fitness, t.continuous),
        NoisyOSE.elitism[M](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.origin,
          t.limit))

    def state = NoisyOSE.state[M]
  }

}

case class NoisyOSE(
  mu: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double],
  limit: Vector[Double],
  origin: (Vector[Double], Vector[Int]) => Vector[Int],
  aggregation: Vector[Vector[Double]] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyOSEOperations {

  def aggregated[I](fitness: I => Vector[Vector[Double]], aggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def promisingReachMap[I](fitness: I => Vector[Double], limit: Vector[Double], origin: I => Vector[Int], population: Vector[I]): Set[Vector[Int]] = {
    val promising = population.filter(i => OSEOperation.patternIsReached(fitness(i), limit))
    promising.map(origin).toSet
  }

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G](
    history: I => Vector[Vector[Double]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    tournamentRounds: Int => Int,
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    limit: Vector[Double])(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Breeding[M, I, G] { population =>
    import cats.implicits._
    import cats.data._

    def genomeOrigin(g: G) = origin(continuousValues(g), discreteValues(g))

    val promisingReachMapValue =
      promisingReachMap[I](
        aggregated(history, aggregation),
        limit,
        i => genomeOrigin(genome(i)),
        population)

    def filterAlreadyReachedAndNeighboursOfPromising(genomes: Vector[G]) =
      for {
        nonReached <- OSEOperation.filterAlreadyReached[M, G](genomeOrigin)(genomes)
      } yield nonReached.filter(g => !promisingReachMapValue.contains(genomeOrigin(g)))

    def adaptiveBreeding(archivedPopulation: Vector[I]) = Breeding[M, I, G] { population =>
      for {
        ranks <- ranking.paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history, aggregation)) apply population
        allRanks = ranks ++ Vector.fill(archivedPopulation.size)(worstParetoRanking)
        continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
        discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
        breeding = applyDynamicOperators[M, I, G](
          tournament(allRanks, tournamentRounds),
          genome andThen continuousValues,
          genome andThen discreteValues,
          continuousOperatorStatistics,
          discreteOperatorStatistics,
          discrete,
          operatorExploration,
          buildGenome) apply (population ++ archivedPopulation)
        offspring <- breeding.flatMap(filterAlreadyReachedAndNeighboursOfPromising).accumulate(lambda)
        sizedOffspringGenomes <- randomTake[M, G](offspring, lambda)
        gs <- clonesReplace[M, I, G](cloneProbability, population, genome, tournament(ranks, tournamentRounds)) apply sizedOffspringGenomes
      } yield gs
    }

    for {
      archived <- archive.get()
      genomes <- adaptiveBreeding(archived).apply(population)
    } yield genomes
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    history: I => Vector[Vector[Double]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    limit: Vector[Double],
    historySize: Int,
    mergeHistories: UncloneStrategy[M, I],
    mu: Int)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Elitism[M, I] { population =>

    import cats.implicits._

    def individualOrigin(i: I) = Function.tupled(origin)(values(i))
    def newlyReaching = {
      def keepNewlyReaching(i: I): M[Option[I]] =
        if (OSEOperation.patternIsReached(aggregated(history, aggregation)(i), limit))
          (reachMap.reached(individualOrigin(i))) map {
            case true => None
            case false if history(i).size >= historySize => Some(i)
            case _ => None
          }
        else (None: Option[I]).pure[M]
      population.flatTraverse(i => keepNewlyReaching(i).map(_.toVector))
    }

    for {
      reaching <- newlyReaching
      _ <- reachMap.setReached(reaching.map(individualOrigin))
      _ <- archive.put(reaching)
      filteredPopulation <- OSEOperation.filterAlreadyReached[M, I] { i: I => Function.tupled(origin)(values(i)) }(population)
      newPopulation <- NoisyNSGA2Operations.elitism[M, I](history, aggregation, values, mergeHistories, mu).apply(filteredPopulation)
    } yield newPopulation
  }
}
