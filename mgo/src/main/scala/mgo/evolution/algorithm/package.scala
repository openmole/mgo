/*
 * Copyright (C) 2015 Guillaume Chérel, Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mgo.evolution.algorithm

import mgo.evolution.*
import breeding.*
import elitism.*
import mgo.tools
import mgo.tools.*
import cats.*
import cats.implicits.*
import cats.data.*
import mgo.tools.execution.Algorithm
import monocle.*
import monocle.syntax.all.*

import scala.reflect.ClassTag
import scala.util.Random


type HitMapState = Map[Vector[Int], Int]
type Archive[I] = IArray[I]
object Archive:
  def empty[I: ClassTag]: Archive[I] = IArray.empty[I]

case class EvolutionState[S](
  generation: Long = 0L,
  evaluated: Long = 0L,
  startTime: Long = java.lang.System.currentTimeMillis(),
  s: S)

//  def state[M[_]: cats.Monad: StartTime: Random: Generation, T](t: T) =
//    for {
//      s <- implicitly[StartTime[M]].get
//      rng <- implicitly[Random[M]].use(identity)
//      g <- implicitly[Generation[M]].get
//    } yield EvolutionState[T](g, s, rng, t)

def randomTake[G](gs: Vector[G], lambda: Int, random: scala.util.Random): Vector[G] = random.shuffle(gs).take(lambda)

def operatorProportions[I](operation: I => Option[Int], is: Vector[I]): Map[Int, Double] =
  is.map { operation }.
    collect { case Some(op) => op }.
    groupBy(identity).
    view.mapValues(_.length.toDouble / is.size).
    toMap

def selectOperator[S, G](
  operators: Vector[(S, G, scala.util.Random) => G],
  opStats: Map[Int, Double],
  exploration: Double): (S, G, Random) => (G, Int) = {

  def allOps =
    operators.zipWithIndex.map {
      case (op, index) => (op, opStats.getOrElse(index, 0.0))
    }

  (s: S, g: G, rng: scala.util.Random) => {
    val (op, i) = drawOperator(allOps, exploration, rng)
    (op(s, g, rng), i)
  }
}

def drawOperator[O](opsAndWeights: Vector[(O, Double)], exploration: Double, rng: scala.util.Random): (O, Int) =
  val explore = rng.nextDouble

  val i =
    if (explore < exploration) rng.nextInt(opsAndWeights.size)
    else multinomialDraw(opsAndWeights.zipWithIndex.map { case ((op, w), i) => (w, i) }, rng)._1

  (opsAndWeights(i)._1, i)

object GenomeVectorDouble {

  def randomGenomes[G](cons: (Vector[Double], Vector[Int]) => G)(mu: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[G => Boolean], rng: scala.util.Random): Vector[G] = {
    def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Vector.fill(genomeLength)(() => rng.nextDouble()).map(_())
    def randomDiscreteValues(genome: Vector[D], rng: scala.util.Random): Vector[Int] = {
      def part(d: D) = tools.randomInt(rng, d)
      genome.map(part)
    }

    def randomG(rng: scala.util.Random) = cons(randomUnscaledContinuousValues(continuous.size, rng), randomDiscreteValues(discrete, rng))

    val rejectValue = reject.getOrElse((_: G) => false)

    def generate(acc: List[G], n: Int): Vector[G] =
      if (n >= mu) acc.toVector
      else {
        val g = randomG(rng)
        if (rejectValue(g)) generate(acc, n)
        else generate(g :: acc, n + 1)
      }

    generate(List(), 0)
  }

  def filterNaN[I, T](values: Vector[I], value: I => T)(implicit cbn: CanBeNaN[T]): Vector[I] =
    values.filter { i => !cbn.isNaN(value(i)) }

  def continuousCrossovers[S]: Vector[GACrossover[S]] =
    Vector(
      sbxC(2.0),
      sbxC(5.0),
      sbxC(20.0))

  def discreteCrossovers[S]: Vector[Crossover[S, (Vector[Int], Vector[Int]), (Vector[Int], Vector[Int])]] =
    Vector(
      binaryCrossover(1.0 / _),
      binaryCrossover(2.0 / _),
      binaryCrossover(_ => 0.1),
      binaryCrossover(_ => 0.5))

  def continuousMutations[S]: Vector[GAMutation[S]] =
    Vector(
      gaussianMutation(mutationRate = 1.0 / _, sigma = 0.0000001),
      gaussianMutation(mutationRate = 1.0 / _, sigma = 0.0001),
      gaussianMutation(mutationRate = 1.0 / _, sigma = 0.1),
      gaussianMutation(mutationRate = _ => 0.1, sigma = 0.1),
      gaussianMutation(mutationRate = _ => 0.5, sigma = 0.5))

  def discreteMutations[S](discrete: Vector[D]): Vector[Mutation[S, Vector[Int], Vector[Int]]] =
    Vector(
      randomMutation(1.0 / _, discrete),
      randomMutation(2.0 / _, discrete),
      randomMutation(_ => 0.1, discrete),
      randomMutation(_ => 0.5, discrete))

  type CrossoverAndMutation[S, G] = (S, G, scala.util.Random) => G

  def continuousCrossoversAndMutations[S]: Vector[CrossoverAndMutation[S, (Vector[Double], Vector[Double])]] =
    for
      c <- continuousCrossovers[S]
      m <- continuousMutations[S]
    yield crossoverAndMutation[S, Vector[Double]](c, m)

  def discreteCrossoversAndMutations[S](discrete: Vector[D]): Vector[CrossoverAndMutation[S, (Vector[Int], Vector[Int])]] =
    for
      c <- discreteCrossovers[S]
      m <- discreteMutations[S](discrete)
    yield crossoverAndMutation[S, Vector[Int]](c, m)

  def crossoverAndMutation[S, G](crossover: Crossover[S, (G, G), (G, G)], mutation: Mutation[S, G, G]): CrossoverAndMutation[S, (G, G)] =
    (s, mates, rng) =>
      val crossed = crossover(s, mates, rng)
      val m1 = mutation(s, crossed._1, rng)
      val m2 = mutation(s, crossed._2, rng)
      (m1, m2)

  def applyOperators[S, I, G](
    crossover: Crossover[S, (G, G), (G, G)],
    mutation: Mutation[S, G, G],
    selection: Selection[S, I],
    genome: I => G)(s: S, population: Vector[I], rng: scala.util.Random): (G, G) =
    val m1 = selection(s, population, rng)
    val m2 = selection(s, population, rng)
    crossoverAndMutation[S, G](crossover, mutation) apply (s, (genome(m1), genome(m2)), rng)

  def applyContinuousDynamicOperators[S, I](
    selection: Selection[S, I],
    genome: I => Vector[Double],
    operatorStatistics: Map[Int, Double],
    operatorExploration: Double): (S, Vector[I], Random) => ((Vector[Double], Vector[Double]), Int) =

    def applyOperator =
      selectOperator(
        continuousCrossoversAndMutations[S],
        operatorStatistics,
        operatorExploration)

    (s: S, population: Vector[I], rng: scala.util.Random) =>
      val m1 = selection(s, population, rng)
      val m2 = selection(s, population, rng)
      applyOperator(s, (genome(m1), genome(m2)), rng)


  def applyDynamicOperators[S, I, G](
    selection: Selection[S, I],
    continuousValues: I => Vector[Double],
    discreteValues: I => Vector[Int],
    continuousOperatorStatistics: Map[Int, Double],
    discreteOperatorStatistics: Map[Int, Double],
    discrete: Vector[D],
    operatorExploration: Double,
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G): (S, Vector[I], Random) => Vector[G] =

    def continuousOperator =
      selectOperator(
        continuousCrossoversAndMutations[S],
        continuousOperatorStatistics,
        operatorExploration)

    def discreteOperator =
      selectOperator(
        discreteCrossoversAndMutations[S](discrete),
        discreteOperatorStatistics,
        operatorExploration)

    (s: S, population: Vector[I], rng: scala.util.Random) =>
      val m1 = selection(s, population, rng)
      val m2 = selection(s, population, rng)
      val c1 = continuousValues(m1)
      val c2 = continuousValues(m2)
      val cOff = continuousOperator(s, (c1, c2), rng)
      val d1 = discreteValues(m1)
      val d2 = discreteValues(m2)
      val dOff = discreteOperator(s, (d1, d2), rng)

      val ((cOff1, cOff2), cop) = cOff
      val ((dOff1, dOff2), dop) = dOff

      import mgo.tools.clamp

      val ng1 = buildGenome(cOff1.map(clamp(_)), Some(cop), dOff1, Some(dop))
      val ng2 = buildGenome(cOff2.map(clamp(_)), Some(cop), dOff2, Some(dop))

      Vector(ng1, ng2)
}

object Aggregation:
  def average(history: Vector[Vector[Double]]): Vector[Double] = history.transpose.map(o => o.sum / o.size)
  def median(history: Vector[Vector[Double]]): Vector[Double] = history.transpose.map(tools.median)

def scaleContinuousValues(values: Vector[Double], genomeComponents: Vector[C]): Vector[Double] =
  (values zip genomeComponents).map((v, c) => v.scale(c))

object CDGenome {

  object DeterministicIndividual:
    case class Individual[P](genome: Genome, phenotype: P, generation: Long, initial: Boolean)

    def individualFitness[P](fitness: P => Vector[Double]): Individual[P] => Vector[Double] = Focus[DeterministicIndividual.Individual[P]](_.phenotype).get _ andThen fitness
    def buildIndividual[P](g: Genome, p: P, generation: Long, initial: Boolean): Individual[P] = Individual(g, p, generation, initial)

    def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): (Genome, Long, Boolean) => Individual[P] =
      deterministic.expression[Genome, P, Individual[P]](
        scaledValues(components),
        buildIndividual[P],
        express)

  object NoisyIndividual:

    def aggregate[P: Manifest](i: Individual[P], aggregation: Vector[P] => Vector[Double], continuous: Vector[C]): (Vector[Double], Vector[Int], Vector[Double], Int) =
      (
        scaleContinuousValues(continuousValues.get(i.genome), continuous),
        i.focus(_.genome) andThen discreteValues get,
        aggregation(vectorPhenotype[P].get(i)),
        i.focus(_.phenotypeHistory).get.size
      )

    case class Individual[P](genome: Genome, phenotypeHistory: Array[P], historyAge: Long, generation: Long, initial: Boolean)

    def buildIndividual[P: Manifest](g: Genome, f: P, generation: Long, initial: Boolean): Individual[P] = Individual[P](g, Array(f), 1, generation, initial)
    def vectorPhenotype[P: Manifest]: PLens[Individual[P], Individual[P], Vector[P], Vector[P]] = Focus[Individual[P]](_.phenotypeHistory) andThen arrayToVectorIso[P]

    def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome, Long, Boolean) => Individual[P] =
      noisy.expression[Genome, Individual[P], P](
        scaledValues(continuous),
        buildIndividual[P])(fitness)


  case class Genome(
    continuousValues: Array[Double],
    continuousOperator: Int,
    discreteValues: Array[Int],
    discreteOperator: Int)

  def buildGenome(
    continuous: Vector[Double],
    continuousOperator: Option[Int],
    discrete: Vector[Int],
    discreteOperator: Option[Int]): Genome =
    Genome(
      continuous.toArray,
      continuousOperator.getOrElse(-1),
      discrete.toArray,
      discreteOperator.getOrElse(-1))

  def continuousValues: PLens[Genome, Genome, Vector[Double], Vector[Double]] = Focus[Genome](_.continuousValues) andThen arrayToVectorIso[Double]
  def continuousOperator: PLens[Genome, Genome, Option[Int], Option[Int]] = Focus[Genome](_.continuousOperator) andThen intToUnsignedIntOption
  def discreteValues: PLens[Genome, Genome, Vector[Int], Vector[Int]] = Focus[Genome](_.discreteValues) andThen arrayToVectorIso[Int]
  def discreteOperator: PLens[Genome, Genome, Option[Int], Option[Int]] = Focus[Genome](_.discreteOperator) andThen intToUnsignedIntOption

  def scaledValues(continuous: Vector[C]) = (g: Genome) =>
    (scaleContinuousValues(continuousValues.get(g), continuous), discreteValues.get(g))

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    GenomeVectorDouble.randomGenomes[Genome]((c, d) => buildGenome(c, None, d, None))(lambda, continuous, discrete, reject, rng)

}

object deterministic:

  private def evaluation[S, G, I](genomes: Vector[G], expression: G => I, parallel: Algorithm.ParallelContext) =
    parallel match
      case Algorithm.Sequential => genomes.map(expression)
      case context: Algorithm.Parallel =>
        import scala.concurrent
        import scala.concurrent.*
        import duration.*

        given concurrent.ExecutionContext = context.executionContext
        val futures = genomes.map(g => Future(expression(g)))
        Await.result(Future.sequence(futures), Duration.Inf)

  def initialPopulation[G, I](
    initialGenomes: Vector[G],
    expression: (G, Long, Boolean) => I,
    parallel: Algorithm.ParallelContext): Vector[I] =
    evaluation(initialGenomes, expression(_, 0, true), parallel)

  def step[S, I, G](
    breeding: Breeding[S, I, G],
    expression: (G, Long, Boolean) => I,
    elitism: Elitism[S, I],
    generation: monocle.Lens[S, Long],
    evaluated: monocle.Lens[S, Long])(s: S, population: Vector[I], rng: scala.util.Random, parallel: Algorithm.ParallelContext): (S, Vector[I]) =
    val newGenomes = breeding(s, population, rng)
    val newPopulation = evaluation(newGenomes, expression(_, generation.get(s), false), parallel)
    val (s2, elitePopulation) = elitism(s, population, newPopulation, rng)
    val s3 = generation.modify(_ + 1)(s2)
    val s4 = evaluated.modify(_ + newGenomes.size)(s3)
    (s4, elitePopulation)

  def expression[G, P, I](
    values: G => (Vector[Double], Vector[Int]),
    build: (G, P, Long, Boolean) => I,
    fitness: (Vector[Double], Vector[Int]) => P) =
    (g: G, generation: Long, initial: Boolean) =>
      val (cs, ds) = values(g)
      build(g, fitness(cs, ds), generation, initial)


object noisy:
  private def evaluation[G, I](expression: (util.Random, G) => I, genomes: Vector[G], rng: Random, parallel: Algorithm.ParallelContext) =
    parallel match
      case Algorithm.Sequential =>
        def evaluate(g: G) = expression(rng, g)
        genomes.map(evaluate)
      case context: Algorithm.Parallel =>
        import scala.concurrent
        import scala.concurrent.*
        import duration.*

        given concurrent.ExecutionContext = context.executionContext

        val futures =
          (genomes zip Iterator.continually(rng.nextLong).map(context.seeder)).map: (g, rng) =>
            Future(expression(rng, g))

        Await.result(Future.sequence(futures), Duration.Inf)

  def initialPopulation[G, I](
    initialGenomes: Vector[G],
    expression: (util.Random, G, Long, Boolean) => I,
    rng: scala.util.Random,
    parallel: Algorithm.ParallelContext): Vector[I] =
    evaluation(expression(_, _, 0, true), initialGenomes, rng, parallel)

  def step[S, I, G](
    breeding: Breeding[S, I, G],
    expression: (util.Random, G, Long, Boolean) => I,
    elitism: Elitism[S, I],
    generation: monocle.Lens[S, Long],
    evaluated: monocle.Lens[S, Long])(s: S, population: Vector[I], rng: scala.util.Random, parallel: Algorithm.ParallelContext): (S, Vector[I]) =

    val newGenomes = breeding(s, population, rng)
    val newPopulation = evaluation(expression(_, _, generation.get(s), false), newGenomes, rng, parallel)

    val (s2, elitePopulation) = elitism(s, population, newPopulation, rng)
    val s3 = generation.modify(_ + 1)(s2)
    val s4 = evaluated.modify(_ + newGenomes.size)(s3)

    (s4, elitePopulation)

  def expression[G, I, P](
    values: G => (Vector[Double], Vector[Int]),
    build: (G, P, Long, Boolean) => I)(phenotype: (util.Random, Vector[Double], Vector[Int]) => P) =
    (rg: util.Random, g: G, generation: Long, initial: Boolean) =>
      val (cs, ds) = values(g)
      build(g, phenotype(rg, cs, ds), generation, initial)



type HitMap = Map[Vector[Int], Int]

