/**
 * Created by Romain Reuillon on 09/01/16.
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
 *
 */
package mgo.algorithm

import mgo._
import mgo.tools._

import elitism._
import breeding._
import mgo.tools.CanBeNaN
import contexts._
import GenomeVectorDouble._
import cats._
import cats.data._
import cats.implicits._
import monocle.macros._
import freedsl.dsl

object NoisyPSE extends niche.Imports {
  type HitMapM[M[_]] = PSE.HitMapM[M]

  import CDGenome._

  @Lenses case class Individual(
    genome: Genome,
    historyAge: Long,
    phenotypeHistory: Array[Array[Double]],
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildIndividual(genome: Genome, phenotype: Vector[Double]) = Individual(genome, 1, Array(phenotype.toArray))
  def vectorPhenotype = Individual.phenotypeHistory composeLens array2ToVectorLens

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: mgo.contexts.HitMap[M, Vector[Int]]) = PSE.state[M]

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation: PSE.HitMapM](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[Vector[Double]] => Vector[Double],
    discrete: Vector[D],
    pattern: Vector[Double] => Vector[Int]): Breeding[M, Individual, Genome] =
    NoisyPSEOperations.adaptiveBreeding[M, Individual, Genome](
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      vectorPhenotype.get _ andThen aggregation andThen pattern,
      buildGenome,
      lambda,
      operatorExploration,
      cloneProbability)

  def elitism[M[_]: cats.Monad: Random: Generation](
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    historySize: Int,
    continuous: Vector[C])(implicit MH: HitMap[M, Vector[Int]]) =
    NoisyPSEOperations.elitism[M, Individual, Vector[Double]](
      i => values(Individual.genome.get(i), continuous),
      vectorPhenotype,
      aggregation,
      pattern,
      Individual.mapped,
      Individual.historyAge,
      historySize)

  def expression(fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    noisy.expression[Genome, Individual](
      values(_, continuous),
      buildIndividual)(fitness)

  def aggregate(i: Individual, aggregation: Vector[Vector[Double]] => Vector[Double], pattern: Vector[Double] => Vector[Int], continuous: Vector[C]) =
    (
      scaleContinuousValues(continuousValues.get(i.genome), continuous),
      Individual.genome composeLens discreteValues get i,
      aggregation(vectorPhenotype.get(i)),
      (vectorPhenotype.get _ andThen aggregation andThen pattern)(i),
      Individual.phenotypeHistory.get(i).size)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], phenotype: Vector[Double], pattern: Vector[Int], replications: Int)

  def result(
    population: Vector[Individual],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    continuous: Vector[C]) =
    population.map {
      i =>
        val (c, d, f, p, r) = aggregate(i, aggregation, pattern, continuous)
        Result(c, d, f, p, r)
    }

  def result(pse: NoisyPSE, population: Vector[Individual]): Vector[Result] =
    result(population, pse.aggregation, pse.pattern, pse.continuous)

  def run[T](rng: util.Random)(f: PSE.PSEImplicits => T): T = PSE.run(rng)(f)
  def run[T](state: EvolutionState[Map[Vector[Int], Int]])(f: PSE.PSEImplicits => T): T = PSE.run(state)(f)

  implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: HitMap[M, Vector[Int]]) = new Algorithm[NoisyPSE, M, Individual, Genome, EvolutionState[Map[Vector[Int], Int]]] {

    def initialPopulation(t: NoisyPSE) =
      noisy.initialPopulation[M, Genome, Individual](
        NoisyPSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyPSE.expression(t.phenotype, t.continuous))

    def step(t: NoisyPSE) =
      noisy.step[M, Individual, Genome](
        NoisyPSE.adaptiveBreeding[M](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          t.pattern),
        NoisyPSE.expression(t.phenotype, t.continuous),
        NoisyPSE.elitism[M](
          t.pattern,
          t.aggregation,
          t.historySize,
          t.continuous))

    def state = NoisyPSE.state[M]

  }
}

case class NoisyPSE(
  lambda: Int,
  phenotype: (util.Random, Vector[Double], Vector[Int]) => Vector[Double],
  pattern: Vector[Double] => Vector[Int],
  aggregation: Vector[Vector[Double]] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyPSEOperations {

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation, I, G](
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    lambda: Int,
    cloneProbability: Double,
    operatorExploration: Double)(implicit MH: HitMap[M, Vector[Int]]) = Breeding[M, I, G] { population =>
    for {
      gs <- PSEOperations.adaptiveBreeding[M, I, G](
        genome,
        continuousValues,
        continuousOperator,
        discreteValues,
        discreteOperator,
        discrete,
        pattern,
        buildGenome,
        lambda,
        operatorExploration) apply population
      withClones <- clonesReplace[M, I, G](cloneProbability, population, genome, randomSelection) apply gs
    } yield withClones
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I, P: CanBeNaN](
    values: I => (Vector[Double], Vector[Int]),
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => P,
    pattern: P => Vector[Int],
    mapped: monocle.Lens[I, Boolean],
    historyAge: monocle.Lens[I, Long],
    historySize: Int)(implicit MH: HitMap[M, Vector[Int]]): Elitism[M, I] = Elitism[M, I] { population =>

    def unclone: UncloneStrategy[M, I] = (is: Vector[I]) => {
      def merged: M[I] = mergeHistories[M, I, P](historyAge, history)(historySize).apply(is)
      merged.map(mapped.set(false))
    }

    for {
      cloneRemoved <- applyCloneStrategy(
        values,
        unclone) apply filterNaN(population, history.get _ andThen aggregation)
      mappedPopulation <- addHits[M, I, Vector[Int]](history.get _ andThen aggregation andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(history.get _ andThen aggregation andThen pattern, maximiseO[M, I, Long](i => history.get(i).size, 1)) apply mappedPopulation
    } yield elite
  }

}