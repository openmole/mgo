package mgo.evolution.algorithm

import freestyle.tagless.tagless
import mgo.evolution._
import mgo.evolution.ranking._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.contexts._
import mgo.tools._
import mgo.tools.execution._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import shapeless._
import mgo.tagtools._

import scala.collection.mutable

object OSE {
  import CDGenome._
  import DeterministicIndividual._
  import mgo.tagtools._

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
    def put(i: Seq[Individual[P]]) = mgo.tagtools.result(archive ++= i)
    def get() = mgo.tagtools.result(archive.toVector)
  }

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad: ReachMap, P](
    lambda: Int,
    operatorExploration: Double,
    discrete: Vector[D],
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    fitness: P => Vector[Double])(implicit archive: Archive[M, Individual[P]]): Breeding[M, Individual[P], Genome] =
    OSEOperation.adaptiveBreeding[M, Individual[P], Genome](
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
      operatorExploration)

  def expression[P](fitness: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(fitness, components)

  def elitism[M[_]: cats.Monad: Random: ReachMap: Generation, P](mu: Int, limit: Vector[Double], origin: (Vector[Double], Vector[Int]) => Vector[Int], components: Vector[C], fitness: P => Vector[Double])(implicit archive: Archive[M, Individual[P]]): Elitism[M, Individual[P]] =
    OSEOperation.elitism[M, Individual[P]](
      individualFitness(fitness),
      limit,
      i => values(Individual.genome.get(i), components),
      origin,
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result[P](state: EvolutionState[OSEState[P]], continuous: Vector[C], fitness: P => Vector[Double]) =
    state.s._1.toVector.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, DeterministicIndividual.individualFitness(fitness)(i))
    }

  object OSEImplicits {
    def apply[P](state: EvolutionState[OSEState[P]]): OSEImplicits[P] =
      OSEImplicits()(
        GenerationInterpreter(state.generation),
        RandomInterpreter(state.random),
        StartTimeInterpreter(state.startTime),
        IOInterpreter(),
        ArchiveInterpreter(state.s._1.to[mutable.Buffer]),
        ReachMapInterpreter(state.s._2.to[mutable.HashSet]),
        SystemInterpreter())
  }

  case class OSEImplicits[P](implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, archiveInterpreter: ArchiveInterpreter[P], reachMapInterpreter: ReachMapInterpreter, systemInterpreter: SystemInterpreter)

  def run[T, S, P](state: EvolutionState[OSEState[P]])(f: OSEImplicits[P] => T): T = f(OSEImplicits(state))

  def state[M[_]: cats.Monad: StartTime: Random: Generation, P](implicit archive: Archive[M, Individual[P]], reachMap: ReachMap[M]) = for {
    map <- reachMap.get()
    arch <- archive.get()
    s <- mgo.evolution.algorithm.state[M, OSEState[P]]((arch.toArray, map.toArray))
  } yield s

  implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: Generation: ReachMap](implicit archive: Archive[M, Individual[Vector[Double]]]): Algorithm[OSE, M, Individual[Vector[Double]], Genome, EvolutionState[OSEState[Vector[Double]]]] = new Algorithm[OSE, M, Individual[Vector[Double]], Genome, EvolutionState[OSEState[Vector[Double]]]] {

    override def initialPopulation(t: OSE) =
      deterministic.initialPopulation[M, Genome, Individual[Vector[Double]]](
        OSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        OSE.expression(t.fitness, t.continuous))

    def step(t: OSE) =
      deterministic.step[M, Individual[Vector[Double]], Genome](
        OSE.adaptiveBreeding[M, Vector[Double]](t.lambda, t.operatorExploration, t.discrete, t.origin, identity),
        OSE.expression(t.fitness, t.continuous),
        OSE.elitism(t.mu, t.limit, t.origin, t.continuous, identity))

    def state = OSE.state[M, Vector[Double]]

  }

  def run[T](rng: util.Random)(f: OSEImplicits[Vector[Double]] => T): T = {
    val state = EvolutionState[OSEState[Vector[Double]]](random = rng, s = (Array.empty, Array.empty))
    run(state)(f)
  }
  def result(ose: OSE, state: EvolutionState[OSEState[Vector[Double]]]): Vector[Result] =
    result[Vector[Double]](state = state, continuous = ose.continuous, fitness = identity)

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

  def filterAlreadyReached[M[_]: cats.Monad: ReachMap, G](origin: G => Vector[Int])(genomes: Vector[G]) = {
    import cats.implicits._
    import cats.data._

    def keepNonReaching(g: G): M[Option[G]] =
      implicitly[ReachMap[M]].reached(origin(g)) map {
        case true => None
        case false => Some(g)
      }

    genomes.flatTraverse(g => keepNonReaching(g).map(_.toVector))
  }

  def adaptiveBreeding[M[_]: cats.Monad: Generation: Random, I, G](
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
    operatorExploration: Double)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Breeding[M, I, G] { population =>
    import cats.implicits._
    import cats.data._

    def adaptiveBreeding(archivedPopulation: Vector[I]) = Breeding[M, I, G] { population =>
      for {
        ranks <- ranking.paretoRankingMinAndCrowdingDiversity[M, I](fitness) apply population
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
        offspring <- breeding.flatMap(filterAlreadyReached[M, G] { g: G => origin(continuousValues(g), discreteValues(g)) }).accumulate(lambda)
        sizedOffspringGenomes <- randomTake[M, G](offspring, lambda)
      } yield sizedOffspringGenomes
    }

    for {
      archived <- archive.get()
      genomes <- adaptiveBreeding(archived).apply(population)
    } yield genomes
  }

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]) =
    (fitness zip limit) forall { case (f, l) => f <= l }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    mu: Int)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Elitism[M, I] { (population, candidates) =>

    val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), fitness)

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
      cloneRemoved.flatTraverse(i => keepNewlyReaching(i).map(_.toVector))
    }

    for {
      reaching <- newlyReaching
      _ <- reachMap.setReached(reaching.map(o))
      _ <- archive.put(reaching)
      filteredPopulation <- filterAlreadyReached[M, I] { i: I => Function.tupled(origin)(values(i)) }(cloneRemoved)
      newPopulation <- NSGA2Operations.elitism[M, I](fitness, values, mu).apply(filteredPopulation, Vector.empty)
    } yield newPopulation
  }

}
