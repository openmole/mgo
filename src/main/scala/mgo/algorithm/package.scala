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
package mgo

import breeding._
import contexts._
import elitism._
import tools._

import cats.implicits._
import cats.data._
import freedsl.tool._

package object algorithm {

  case class EvolutionState[S](
    generation: Long = 0,
    startTime: Long = System.currentTimeMillis(),
    random: util.Random = newRNG(System.currentTimeMillis()),
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

  def selectOperator[M[_]: cats.Monad, G](operators: Vector[Kleisli[M, G, G]], opStats: Map[Int, Double], exploration: Double)(implicit MR: Random[M]) = {
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

  def deterministicStep[M[_]: cats.Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = Kleisli { population =>
    for {
      newGenomes <- breeding(population)
      newPopulation = newGenomes.map(expression)
      mergedPopulation = muPlusLambda(population, newPopulation)
      elitePopulation <- elitism(mergedPopulation)
    } yield elitePopulation
  }

  def noisyStep[M[_]: cats.Monad: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(util.Random, G), I],
    elitism: Elitism[M, I])(implicit randomM: Random[M]): Kleisli[M, Vector[I], Vector[I]] = Kleisli { population =>

    def evaluate(g: G) = randomM.use { rng => expression(rng, g) }

    for {
      newGenomes <- breeding(population)
      newPopulation <- newGenomes.traverse { evaluate }
      mergedPopulation = muPlusLambda(population, newPopulation)
      elitePopulation <- elitism(mergedPopulation)
    } yield elitePopulation
  }

  def deterministicInitialPopulation[M[_]: cats.Monad, G, I](
    initialGenomes: M[Vector[G]],
    expression: Expression[G, I]) =
    for {
      gs <- initialGenomes
    } yield gs.map(expression)

  //  trait ParallelRandom[M[_]] {
  //    /**
  //     * Returns a new random number generator that is independant from the one in M, useful for parallel computations.
  //     * Implementations of this function must use a random number generator contained in M in order to produce the Random returned, and update the original
  //     * random number generator in an independant manner so that it is never used twice (and allows for safe parallelisation).
  //     */
  //    def split: M[util.Random]
  //  }

  def stochasticInitialPopulation[M[_]: cats.Monad, G, I](
    initialGenomes: M[Vector[G]],
    expression: Expression[(util.Random, G), I])(implicit random: Random[M]) =
    for {
      genomes <- initialGenomes
      initialIndividuals <- genomes.traverse(g => random.use(rng => expression(rng, g)))
    } yield initialIndividuals

  object GenomeVectorDouble {
    def randomGenomes[M[_]: cats.Monad](n: Int, genomeLength: Int)(
      implicit randomM: Random[M]): M[Vector[Vector[Double]]] = {
      def genome = randomM.nextDouble.repeat(genomeLength)
      Vector.fill(n)(genome).sequence
    }

    def randomGenomes[M[_]: cats.Monad: Random, G](cons: (Vector[Double], Option[Int]) => G)(mu: Int, genomeSize: Int): M[Vector[G]] =
      for {
        values <- randomGenomes[M](mu, genomeSize)
        gs = values.map { (vs: Vector[Double]) => cons(vs, None) }
      } yield gs

    //    def clamp[G](lens: monocle.Lens[G, Vector[Double]]) =
    //      (gs: Vector[G]) => gs.map(lens.modify(_ map { x: Double => scala.math.max(0.0, scala.math.min(1.0, x)) }))

    def filterNaN[I, T](values: Vector[I], value: I => T)(implicit cbn: CanBeNaN[T]) =
      values.filter { i => !cbn.isNaN(value(i)) }

    def crossovers[M[_]: cats.Monad: Random]: Vector[Crossover[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
      Vector(
        replicatePairC(blxC(0.1)),
        replicatePairC(blxC(0.5)),
        replicatePairC(blxC(2.0)),
        sbxC(0.1),
        sbxC(0.5),
        sbxC(2.0)
      )

    def mutations[M[_]: cats.Monad: Random]: Vector[Mutation[M, Vector[Double], Vector[Double]]] =
      Vector(
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.001),
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.01),
        bgaM(mutationRate = 2.0 / _, mutationRange = 0.1),
        bgaM(mutationRate = _ => 0.5, mutationRange = 0.5)
      )

    def crossoversAndMutations[M[_]: cats.Monad: Random]: Vector[Kleisli[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
      for {
        c <- crossovers[M]
        m <- mutations[M]
      } yield {
        Kleisli((mates: (Vector[Double], Vector[Double])) =>
          for {
            crossed <- c.run(mates)
            m1 <- m.run(crossed._1)
            m2 <- m.run(crossed._2)
          } yield (m1, m2))
      }

    def applyDynamicOperator[M[_]: cats.Monad: Random, I](selection: M[I], genome: I => Vector[Double], operatorStatistics: Map[Int, Double], operatorExploration: Double) = {
      def applyOperator =
        selectOperator[M, (Vector[Double], Vector[Double])](
          crossoversAndMutations[M],
          operatorStatistics,
          operatorExploration
        )

      for {
        m1 <- selection
        m2 <- selection
        offspring <- applyOperator apply ((genome(m1), genome(m2)))
      } yield offspring

    }
  }

}
