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

import scala.collection.mutable.ListBuffer

object OSE {
  import CDGenome._
  import DeterministicIndividual._
  import freedsl.dsl._

  type OSEState = Seq[Individual]

  @tagless trait IndividualArchive {
    def put(i: Seq[Individual]): FS[Unit]
    def get(): FS[Vector[Individual]]
  }

  implicit def archiveConvert[M[_]](implicit vhm: IndividualArchive[M]) = new Archive[M, Individual] {
    def put(i: Seq[Individual]) = vhm.put(i)
    def get() = vhm.get()
  }

  case class ArchiveInterpreter(var archive: ListBuffer[Individual]) extends IndividualArchive.Handler[Evaluated] {
    def put(i: Seq[Individual]) = result(archive ++= i)
    def get() = result(archive.toVector)
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

  //  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])
  //
  //  //  def result(population: Vector[Individual], continuous: Vector[C]) =
  //  //    keepFirstFront(population, vectorFitness.get).map { i =>
  //  //      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, i.fitness.toVector)
  //  //    }
  //  //
  //  //  def result(nsga2: NSGA2, population: Vector[Individual]): Vector[Result] = result(population, nsga2.continuous)
  //  //
  //  //  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())
  //

  //  object OSEImplicits {
  //    def apply(state: EvolutionState[Map[Vector[Int], Int]]): OSEImplicits =
  //      PSEImplicits()(GenerationInterpreter(state.generation), RandomInterpreter(state.random), StartTimeInterpreter(state.startTime), IOInterpreter(), HitMapInterpreter(state.s), SystemInterpreter())
  //  }

  case class OSEImplicits(implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, archiveInterpreter: ArchiveInterpreter, reachMapInterpreter: ReachMapInterpreter, systemInterpreter: SystemInterpreter)
  //
  //  def run[T](rng: util.Random)(f: OSEImplicits => T): T = contexts.run(rng)(f)
  //  def run[T](state: EvolutionState[Unit])(f: OSEImplicits => T): T = contexts.run(state)(f)

}

case class OSE(
  mu: Int,
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  pattern: Vector[Double],
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
