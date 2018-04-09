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
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freedsl.tool._
import shapeless._

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object OSE {
  import CDGenome._
  import DeterministicIndividual._
  import freedsl.dsl._

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

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad: ReachMap](lambda: Int, operatorExploration: Double, discrete: Vector[D], origin: (Vector[Double], Vector[Int]) => Vector[Int])(implicit archive: Archive[M, Individual]): Breeding[M, Individual, Genome] =
    OSEOperation.adaptiveBreeding[M, Individual, Genome](
      vectorFitness,
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      origin,
      buildGenome,
      _ => 1,
      lambda,
      operatorExploration)

  def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C]): Genome => Individual =
    DeterministicIndividual.expression(fitness, components)

  def elitism[M[_]: cats.Monad: Random: ReachMap: Generation](mu: Int, limit: Vector[Double], origin: (Vector[Double], Vector[Int]) => Vector[Int], components: Vector[C])(implicit archive: Archive[M, Individual]): Elitism[M, Individual] =
    OSEOperation.elitism[M, Individual](
      vectorFitness.get,
      limit,
      i => values(Individual.genome.get(i), components),
      origin,
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result(state: EvolutionState[OSEState], continuous: Vector[C]) =
    state.s._1.toVector.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
    }

  def result(ose: OSE, state: EvolutionState[OSEState]): Vector[Result] =
    result(state = state, continuous = ose.continuous)

  object OSEImplicits {
    def apply(state: EvolutionState[OSEState]): OSEImplicits =
      OSEImplicits()(
        GenerationInterpreter(state.generation),
        RandomInterpreter(state.random),
        StartTimeInterpreter(state.startTime),
        IOInterpreter(),
        ArchiveInterpreter(state.s._1.to[mutable.Buffer]),
        ReachMapInterpreter(state.s._2.to[mutable.HashSet]),
        SystemInterpreter())
  }

  case class OSEImplicits(implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, archiveInterpreter: ArchiveInterpreter, reachMapInterpreter: ReachMapInterpreter, systemInterpreter: SystemInterpreter)

  def run[T](rng: util.Random)(f: OSEImplicits => T): T = {
    val state = EvolutionState[OSEState](random = rng, s = (Array.empty, Array.empty))
    run(state)(f)
  }

  def run[T, S](state: EvolutionState[OSEState])(f: OSEImplicits => T): T = f(OSEImplicits(state))

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit archive: Archive[M, Individual], reachMap: ReachMap[M]) = for {
    map <- reachMap.get()
    arch <- archive.get()
    s <- mgo.algorithm.state[M, OSEState]((arch.toArray, map.toArray))
  } yield s

  implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: Generation: ReachMap](implicit archive: Archive[M, Individual]): Algorithm[OSE, M, Individual, Genome, EvolutionState[OSEState]] = new Algorithm[OSE, M, Individual, Genome, EvolutionState[OSEState]] {

    override def initialPopulation(t: OSE) =
      deterministic.initialPopulation[M, Genome, Individual](
        OSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        OSE.expression(t.fitness, t.continuous))

    def step(t: OSE) =
      deterministic.step[M, Individual, Genome](
        OSE.adaptiveBreeding[M](t.lambda, t.operatorExploration, t.discrete, t.origin),
        OSE.expression(t.fitness, t.continuous),
        OSE.elitism(t.mu, t.limit, t.origin, t.continuous))

    def state = OSE.state[M]

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
  operatorExploration: Double = 0.1)

object OSEOperation {

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G](
    fitness: monocle.Lens[I, Vector[Double]],
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
    operatorExploration: Double)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Breeding[M, I, G] { population =>
    import cats.implicits._
    import cats.data._

    def filterAlreadyReached(genomes: Vector[G]) = {
      def keepNonReaching(g: G): M[Option[G]] =
        reachMap.reached(origin(continuousValues(g), discreteValues(g))) map {
          case true => None
          case false => Some(g)
        }
      genomes.flatTraverse(g => keepNonReaching(g).map(_.toVector))
    }

    def adaptiveBreeding = Breeding[M, I, G] { population =>
      for {
        ranks <- ranking.paretoRankingMinAndCrowdingDiversity[M, I](fitness.get) apply population
        continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
        discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
        breeding = applyDynamicOperators[M, I, G](
          tournament(ranks, tournamentRounds),
          genome andThen continuousValues,
          genome andThen discreteValues,
          continuousOperatorStatistics,
          discreteOperatorStatistics,
          discrete,
          operatorExploration,
          buildGenome) apply population
        offspring <- breeding.flatMap(filterAlreadyReached).accumulate(lambda)
        sizedOffspringGenomes <- randomTake[M, G](offspring, lambda)
      } yield sizedOffspringGenomes
    }

    for {
      archived <- archive.get()
      additionalIndividuals = archived.map { i => fitness.modify(_.map(_ => Double.PositiveInfinity))(i) }
      genomes <- adaptiveBreeding.apply(population ++ additionalIndividuals)
    } yield genomes
  }

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]) =
    (fitness zip limit) forall { case (f, l) => f <= l }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    mu: Int)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Elitism[M, I] { population =>

    import cats.implicits._

    def o(i: I) = Function.tupled(origin)(values(i))

    def newlyReaching = {
      def keepNewlyReaching(i: I): M[Option[I]] =
        if (patternIsReached(fitness(i), limit))
          (reachMap.reached(o(i))) map {
            case true => None
            case false => Some(i)
          }
        else (None: Option[I]).pure[M]
      population.flatTraverse(i => keepNewlyReaching(i).map(_.toVector))
    }

    for {
      reaching <- newlyReaching
      _ <- reachMap.setReached(reaching.map(o))
      _ <- archive.put(reaching)
      newPopulation <- NSGA2Operations.elitism[M, I](fitness, values, mu).apply(population)
    } yield newPopulation
  }

}
