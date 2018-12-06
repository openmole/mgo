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

  type OSEState[P] = (Array[Individual[P]], Array[Vector[Int]])

  @tagless trait IndividualArchive[P] {
    def put(i: Seq[Individual[P]]): FS[Unit]
    def get(): FS[Vector[Individual[P]]]
  }

  implicit def archiveConvert[M[_], P](implicit vhm: IndividualArchive[M, P]) = new Archive[M, Individual[P]] {
    def put(i: Seq[Individual[P]]) = vhm.put(i)
    def get() = vhm.get()
  }

  case class ArchiveInterpreter[P](val archive: collection.mutable.Buffer[Individual[P]]) extends IndividualArchive.Handler[Evaluated, P] {
    def put(i: Seq[Individual[P]]) = freedsl.dsl.result(archive ++= i)
    def get() = freedsl.dsl.result(archive.toVector)
  }

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation: ReachMap, P: Manifest](lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[P] => Vector[Double], discrete: Vector[D], origin: (Vector[Double], Vector[Int]) => Vector[Int], limit: Vector[Double])(implicit archive: Archive[M, Individual[P]]): Breeding[M, Individual[P], Genome] =
    NoisyOSEOperations.adaptiveBreeding[M, Individual[P], Genome, P](
      vectorFitness[P].get,
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

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    NoisyIndividual.expression[P](fitness, continuous)

  def elitism[M[_]: cats.Monad: Random: Generation: ReachMap, P: Manifest](mu: Int, historySize: Int, aggregation: Vector[P] => Vector[Double], components: Vector[C], origin: (Vector[Double], Vector[Int]) => Vector[Int], limit: Vector[Double])(implicit archive: Archive[M, Individual[P]]): Elitism[M, Individual[P]] =
    NoisyOSEOperations.elitism[M, Individual[P], P](
      vectorFitness[P].get,
      aggregation,
      i => values(Individual.genome.get(i), components),
      origin,
      limit,
      historySize,
      mergeHistories(Individual.historyAge[P], vectorFitness[P])(historySize),
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], replications: Int)

  def result[P: Manifest](state: EvolutionState[OSEState[P]], population: Vector[Individual[P]], aggregation: Vector[P] => Vector[Double], continuous: Vector[C], limit: Vector[Double]) = {
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

  def result[P: Manifest](noisyOSE: NoisyOSE[P], state: EvolutionState[OSEState[P]], population: Vector[Individual[P]]): Vector[Result] =
    result[P](state, population, noisyOSE.aggregation, noisyOSE.continuous, noisyOSE.limit)

  def state[M[_]: cats.Monad: StartTime: Random: Generation, P](implicit archive: Archive[M, Individual[P]], reachMap: ReachMap[M]) = for {
    map <- reachMap.get()
    arch <- archive.get()
    s <- mgo.algorithm.state[M, OSEState[P]]((arch.toArray, map.toArray))
  } yield s

  object OSEImplicits {
    def apply[P](state: EvolutionState[OSEState[P]]): OSEImplicits[P] =
      OSEImplicits()(
        GenerationInterpreter(state.generation),
        RandomInterpreter(state.random),
        StartTimeInterpreter(state.startTime),
        IOInterpreter(),
        ArchiveInterpreter[P](state.s._1.to[collection.mutable.Buffer]),
        ReachMapInterpreter(state.s._2.to[collection.mutable.HashSet]),
        SystemInterpreter())
  }

  case class OSEImplicits[P](implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, archiveInterpreter: ArchiveInterpreter[P], reachMapInterpreter: ReachMapInterpreter, systemInterpreter: SystemInterpreter)

  def run[T](rng: util.Random)(f: OSEImplicits[Vector[Double]] => T): T = {
    val state = EvolutionState[OSEState[Vector[Double]]](random = rng, s = (Array.empty, Array.empty))
    run(state)(f)
  }

  def runP[T, S, P](state: EvolutionState[OSEState[P]])(f: OSEImplicits[P] => T): T = f(OSEImplicits(state))

  def runP[T, P](rng: util.Random)(f: OSEImplicits[P] => T): T = {
    val state = EvolutionState[OSEState[P]](random = rng, s = (Array.empty, Array.empty))
    run(state)(f)
  }

  def run[T, S, P](state: EvolutionState[OSEState[P]])(f: OSEImplicits[P] => T): T = f(OSEImplicits(state))

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime: ReachMap, P: Manifest](implicit archive: Archive[M, Individual[P]]): Algorithm[NoisyOSE[P], M, Individual[P], Genome, EvolutionState[OSEState[P]]] = new Algorithm[NoisyOSE[P], M, Individual[P], Genome, EvolutionState[OSEState[P]]] {
    def initialPopulation(t: NoisyOSE[P]) =
      noisy.initialPopulation[M, Genome, Individual[P]](
        NoisyOSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyOSE.expression[P](t.fitness, t.continuous))

    def step(t: NoisyOSE[P]): Kleisli[M, Vector[Individual[P]], Vector[Individual[P]]] =
      noisy.step[M, Individual[P], Genome](
        NoisyOSE.adaptiveBreeding[M, P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          t.origin,
          t.limit),
        NoisyOSE.expression(t.fitness, t.continuous),
        NoisyOSE.elitism[M, P](
          t.mu,
          t.historySize,
          t.aggregation,
          t.continuous,
          t.origin,
          t.limit))

    def state = NoisyOSE.state[M, P]
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
  operatorExploration: Double = 0.1)

object NoisyOSEOperations {

  def aggregated[I, P](fitness: I => Vector[P], aggregation: Vector[P] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def promisingReachMap[I](fitness: I => Vector[Double], limit: Vector[Double], origin: I => Vector[Int], population: Vector[I]): Set[Vector[Int]] = {
    val promising = population.filter(i => OSEOperation.patternIsReached(fitness(i), limit))
    promising.map(origin).toSet
  }

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G, P](
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

  def elitism[M[_]: cats.Monad: Random: Generation, I, P](
    history: I => Vector[P],
    aggregation: Vector[P] => Vector[Double],
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
      newPopulation <- NoisyNSGA2Operations.elitism[M, I, P](history, aggregation, values, mergeHistories, mu).apply(filteredPopulation)
    } yield newPopulation
  }
}
