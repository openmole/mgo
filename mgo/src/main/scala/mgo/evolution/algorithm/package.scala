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
package mgo.evolution

import breeding._
import contexts._
import elitism._
import mgo.tools
import mgo.tools._

import cats._
import cats.implicits._
import cats.data._

package object algorithm {

  type HitMapState = Map[Vector[Int], Int]

  case class EvolutionState[S](
    generation: Long = 0,
    startTime: Long = java.lang.System.currentTimeMillis(),
    random: util.Random = newRNG(java.lang.System.currentTimeMillis()),
    s: S)

  def state[M[_]: cats.Monad: StartTime: Random: Generation, T](t: T) =
    for {
      s <- implicitly[StartTime[M]].get
      rng <- implicitly[Random[M]].use(identity)
      g <- implicitly[Generation[M]].get
    } yield EvolutionState[T](g, s, rng, t)

  def randomTake[M[_]: cats.Monad, G](gs: Vector[G], lambda: Int)(implicit randomM: Random[M]) =
    randomM.shuffle(gs).map { _.take(lambda) }

  def operatorProportions[I](operation: I => Option[Int], is: Vector[I]) =
    is.map { operation }.
      collect { case Some(op) => op }.
      groupBy(identity).
      mapValues(_.length.toDouble / is.size)

  def selectOperator[M[_]: cats.Monad, G](
    operators: Vector[Kleisli[M, G, G]],
    opStats: Map[Int, Double],
    exploration: Double)(implicit MR: Random[M]) = {

    def allOps =
      operators.zipWithIndex.map {
        case (op, index) => (op, opStats.getOrElse(index, 0.0))
      }

    probabilisticOperatorB[M, G](allOps, exploration)
  }

  def probabilisticOperatorB[M[_]: cats.Monad, G](opsAndWeights: Vector[(Kleisli[M, G, G], Double)], exploration: Double)(implicit randomM: Random[M]): Kleisli[M, G, (G, Int)] =
    Kleisli((mates: G) => {
      for {
        explore <- randomM.nextDouble
        op <- if (explore < exploration) randomM.nextInt(opsAndWeights.size)
        else randomM.multinomial(opsAndWeights.zipWithIndex.map { case ((op, w), i) => (i, w) })
        g <- opsAndWeights(op)._1.run(mates)
      } yield (g, op)
    })

  object GenomeVectorDouble {

    def randomUnscaledContinuousValues[M[_]: cats.Monad](n: Int, genomeLength: Int)(
      implicit
      randomM: Random[M]): M[Vector[Vector[Double]]] = {
      def genome = randomM.use(rng => Vector.fill(genomeLength)(rng.nextDouble()))
      Vector.fill(n)(genome).sequence
    }

    def randomDiscreteValues[M[_]: cats.Monad](n: Int, genome: Vector[D])(
      implicit
      randomM: Random[M]): M[Vector[Vector[Int]]] = {
      def part(d: D) = randomM.use(rng => tools.randomInt(rng, d))
      def random = genome.map(part).sequence
      Vector.fill(n)(random).sequence
    }

    def randomGenomes[M[_]: cats.Monad: Random, G](cons: (Vector[Double], Vector[Int]) => G)(mu: Int, continuous: Vector[C], discrete: Vector[D]): M[Vector[G]] =
      for {
        discreteGenomes <- randomDiscreteValues[M](mu, discrete)
        continuousGenomes <- randomUnscaledContinuousValues[M](mu, continuous.size)
      } yield (continuousGenomes zip discreteGenomes).map(Function.tupled(cons))

    def filterNaN[I, T](values: Vector[I], value: I => T)(implicit cbn: CanBeNaN[T]) =
      values.filter { i => !cbn.isNaN(value(i)) }

    def continuousCrossovers[M[_]: cats.Monad: Random]: Vector[Crossover[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
      Vector(
        sbxC(2.0),
        sbxC(5.0),
        sbxC(20.0))

    def discreteCrossovers[M[_]: cats.Monad: Random]: Vector[Crossover[M, (Vector[Int], Vector[Int]), (Vector[Int], Vector[Int])]] =
      Vector(
        binaryCrossover(1.0 / _),
        binaryCrossover(2.0 / _),
        binaryCrossover(_ => 0.1),
        binaryCrossover(_ => 0.5))

    def continuousMutations[M[_]: cats.Monad: Random]: Vector[Mutation[M, Vector[Double], Vector[Double]]] =
      Vector(
        gaussianMutation(mutationRate = 1.0 / _, sigma = 0.0000001),
        gaussianMutation(mutationRate = 1.0 / _, sigma = 0.0001),
        gaussianMutation(mutationRate = 1.0 / _, sigma = 0.1),
        gaussianMutation(mutationRate = _ => 0.1, sigma = 0.1),
        gaussianMutation(mutationRate = _ => 0.5, sigma = 0.5))

    def discreteMutations[M[_]: cats.Monad: Random](discrete: Vector[D]): Vector[Mutation[M, Vector[Int], Vector[Int]]] =
      Vector(
        randomMutation(1.0 / _, discrete),
        randomMutation(2.0 / _, discrete),
        randomMutation(_ => 0.1, discrete),
        randomMutation(_ => 0.5, discrete))

    type CrossoverAndMutation[M[_], G] = Kleisli[M, (G, G), (G, G)]

    def continuousCrossoversAndMutations[M[_]: cats.Monad: Random]: Vector[CrossoverAndMutation[M, Vector[Double]]] =
      for {
        c <- continuousCrossovers[M]
        m <- continuousMutations[M]
      } yield crossoverAndMutation(c, m)

    def discreteCrossoversAndMutations[M[_]: cats.Monad: Random](discrete: Vector[D]): Vector[CrossoverAndMutation[M, Vector[Int]]] =
      for {
        c <- discreteCrossovers[M]
        m <- discreteMutations[M](discrete)
      } yield crossoverAndMutation(c, m)

    def crossoverAndMutation[M[_]: cats.Monad, G](crossover: Crossover[M, (G, G), (G, G)], mutation: Mutation[M, G, G]) =
      Kleisli[M, (G, G), (G, G)] { mates =>
        for {
          crossed <- crossover.run(mates)
          m1 <- mutation.run(crossed._1)
          m2 <- mutation.run(crossed._2)
        } yield (m1, m2)
      }

    def applyOperators[M[_]: cats.Monad: Random, I, G](
      crossover: Crossover[M, (G, G), (G, G)],
      mutation: Mutation[M, G, G],
      selection: Selection[M, I],
      genome: I => G) =
      Kleisli { population: Vector[I] =>
        for {
          m1 <- selection apply population
          m2 <- selection apply population
          offspring <- crossoverAndMutation[M, G](crossover, mutation) apply ((genome(m1), genome(m2)))
        } yield offspring
      }

    def applyContinuousDynamicOperators[M[_]: cats.Monad: Random, I](
      selection: Selection[M, I],
      genome: I => Vector[Double],
      operatorStatistics: Map[Int, Double],
      operatorExploration: Double) = {

      def applyOperator =
        selectOperator(
          continuousCrossoversAndMutations[M],
          operatorStatistics,
          operatorExploration)

      Kleisli { population: Vector[I] =>
        for {
          m1 <- selection apply population
          m2 <- selection apply population
          offspring <- applyOperator apply ((genome(m1), genome(m2)))
        } yield offspring
      }
    }

    def applyDynamicOperators[M[_]: cats.Monad: Random, I, G](
      selection: Selection[M, I],
      continuousValues: I => Vector[Double],
      discreteValues: I => Vector[Int],
      continuousOperatorStatistics: Map[Int, Double],
      discreteOperatorStatistics: Map[Int, Double],
      discrete: Vector[D],
      operatorExploration: Double,
      buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G) = {

      def continuousOperator =
        selectOperator(
          continuousCrossoversAndMutations[M],
          continuousOperatorStatistics,
          operatorExploration)

      def discreteOperator =
        selectOperator(
          discreteCrossoversAndMutations[M](discrete),
          discreteOperatorStatistics,
          operatorExploration)

      Kleisli { population: Vector[I] =>
        for {
          m1 <- selection apply population
          m2 <- selection apply population
          c1 = continuousValues(m1)
          c2 = continuousValues(m2)
          cOff <- continuousOperator apply (c1, c2)
          d1 = discreteValues(m1)
          d2 = discreteValues(m2)
          dOff <- discreteOperator apply (d1, d2)
        } yield {
          val ((cOff1, cOff2), cop) = cOff
          val ((dOff1, dOff2), dop) = dOff

          import mgo.tools.clamp

          val ng1 = buildGenome(cOff1.map(clamp(_)), Some(cop), dOff1, Some(dop))
          val ng2 = buildGenome(cOff2.map(clamp(_)), Some(cop), dOff2, Some(dop))

          Vector(ng1, ng2)
        }
      }
    }
  }

  object Operators {
    implicit def pairToManual[M[_]](crossover: GACrossover[M], mutation: GAMutation[M]) = ManualOperators(crossover, mutation)
  }

  sealed trait Operators[M[_]]
  case class AdaptiveOperators[M[_]](operatorExploration: Double = 0.1) extends Operators[M]
  case class ManualOperators[M[_]](crossover: GACrossover[M], mutation: GAMutation[M]) extends Operators[M]

  def averageAggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  def scaleContinuousValues(values: Vector[Double], genomeComponents: Vector[C]) =
    (values zip genomeComponents).map { case (v, c) => v.scale(c) }

  object CDGenome {

    import monocle.macros._

    object DeterministicIndividual {
      @Lenses case class Individual(genome: Genome, fitness: Array[Double])
      def vectorFitness = Individual.fitness composeLens arrayToVectorLens
      def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray)

      def expression(fitness: (Vector[Double], Vector[Int]) => Vector[Double], components: Vector[C]): Genome => Individual =
        deterministic.expression[Genome, Individual](
          values(_, components),
          buildIndividual,
          fitness)

    }

    object NoisyIndividual {

      def aggregate[P: Manifest](i: Individual[P], aggregation: Vector[P] => Vector[Double], continuous: Vector[C]) =
        (
          scaleContinuousValues(continuousValues.get(i.genome), continuous),
          Individual.genome composeLens discreteValues get i,
          aggregation(vectorFitness[P].get(i)),
          Individual.fitnessHistory.get(i).size)

      @Lenses case class Individual[P](genome: Genome, historyAge: Long, fitnessHistory: Array[P])
      def buildIndividual[P: Manifest](g: Genome, f: P) = Individual[P](g, 1, Array(f))
      def vectorFitness[P: Manifest] = Individual.fitnessHistory[P] composeLens arrayToVectorLens[P]

      def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
        noisy.expression[Genome, Individual[P], P](
          values(_, continuous),
          buildIndividual[P])(fitness)
    }

    @Lenses case class Genome(
      continuousValues: Array[Double],
      continuousOperator: Int,
      discreteValues: Array[Int],
      discreteOperator: Int)

    def buildGenome(
      continuous: Vector[Double],
      continuousOperator: Option[Int],
      discrete: Vector[Int],
      discreteOperator: Option[Int]) =
      Genome(
        continuous.toArray,
        continuousOperator.getOrElse(-1),
        discrete.toArray,
        discreteOperator.getOrElse(-1))

    def continuousValues = Genome.continuousValues composeLens arrayToVectorLens
    def continuousOperator = Genome.continuousOperator composeLens intToUnsignedIntOption
    def discreteValues = Genome.discreteValues composeLens arrayToVectorLens
    def discreteOperator = Genome.discreteOperator composeLens intToUnsignedIntOption

    def values(g: Genome, continuous: Vector[C]) =
      (scaleContinuousValues(continuousValues.get(g), continuous), discreteValues.get(g))

    def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]): M[Vector[Genome]] =
      GenomeVectorDouble.randomGenomes[M, Genome]((c, d) => buildGenome(c, None, d, None))(lambda, continuous, discrete)

  }

  object deterministic {
    def initialPopulation[M[_]: cats.Monad, G, I](
      initialGenomes: M[Vector[G]],
      expression: G => I) =
      for {
        gs <- initialGenomes
      } yield gs.map(expression)

    def step[M[_]: cats.Monad: Random: Generation, I, G](
      breeding: Breeding[M, I, G],
      expression: G => I,
      elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = Kleisli { population =>
      for {
        newGenomes <- breeding(population)
        newPopulation = newGenomes.map(expression)
        elitePopulation <- elitism(population, newPopulation)
        _ <- incrementGeneration
      } yield elitePopulation
    }

    def expression[G, I](
      values: G => (Vector[Double], Vector[Int]),
      build: (G, Vector[Double]) => I,
      fitness: (Vector[Double], Vector[Int]) => Vector[Double]): G => I = {
      (g: G) =>
        val (cs, ds) = values(g)
        build(g, fitness(cs, ds))
    }

  }

  object noisy {
    def initialPopulation[M[_]: cats.Monad, G, I](
      initialGenomes: M[Vector[G]],
      expression: (util.Random, G) => I)(implicit random: Random[M]) =
      for {
        genomes <- initialGenomes
        initialIndividuals <- genomes.traverse(g => random.use(rng => expression(rng, g)))
      } yield initialIndividuals

    def step[M[_]: cats.Monad: Generation, I, G](
      breeding: Breeding[M, I, G],
      expression: (util.Random, G) => I,
      elitism: Elitism[M, I])(implicit randomM: Random[M]): Kleisli[M, Vector[I], Vector[I]] = Kleisli { population =>

      def evaluate(g: G) = randomM.use { rng => expression(rng, g) }

      for {
        newGenomes <- breeding(population)
        newPopulation <- newGenomes.traverse { evaluate }
        elitePopulation <- elitism(population, newPopulation)
        _ <- incrementGeneration
      } yield elitePopulation
    }

    def expression[G, I, P](
      values: G => (Vector[Double], Vector[Int]),
      build: (G, P) => I)(phenotype: (util.Random, Vector[Double], Vector[Int]) => P): (util.Random, G) => I = {
      case (rg, g) =>
        val (cs, ds) = values(g)
        build(g, phenotype(rg, cs, ds))
    }

  }

}
