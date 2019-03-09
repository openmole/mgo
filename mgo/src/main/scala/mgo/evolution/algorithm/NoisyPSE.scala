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
package mgo.evolution.algorithm

import mgo.evolution._
import mgo.tools._
import mgo.tools.execution._

import elitism._
import breeding._
import mgo.tools.CanBeNaN
import contexts._
import GenomeVectorDouble._
import cats._
import cats.data._
import cats.implicits._
import monocle.macros._
import mgo.tagtools._

object NoisyPSE {

  import CDGenome._

  @Lenses case class Individual[P](
    genome: Genome,
    historyAge: Long,
    phenotypeHistory: Array[P],
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildIndividual[P: Manifest](genome: Genome, phenotype: P) = Individual(genome, 1, Array(phenotype))
  def vectorPhenotype[P: Manifest] = Individual.phenotypeHistory[P] composeLens arrayToVectorLens

  def state[M[_]: cats.Monad: StartTime: Random: Generation: HitMap] = PSE.state[M]

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation: HitMap, P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    discrete: Vector[D],
    pattern: Vector[Double] => Vector[Int]): Breeding[M, Individual[P], Genome] =
    NoisyPSEOperations.adaptiveBreeding[M, Individual[P], Genome](
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      vectorPhenotype[P].get _ andThen aggregation andThen pattern,
      buildGenome,
      lambda,
      operatorExploration,
      cloneProbability)

  def elitism[M[_]: cats.Monad: Random: HitMap: Generation, P: CanBeNaN: Manifest](
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[P] => Vector[Double],
    historySize: Int,
    continuous: Vector[C]) =
    NoisyPSEOperations.elitism[M, Individual[P], P](
      i => values(Individual.genome.get(i), continuous),
      vectorPhenotype[P],
      aggregation,
      pattern,
      Individual.mapped,
      Individual.historyAge,
      historySize)

  def expression[P: Manifest](fitness: (util.Random, Vector[Double], Vector[Int]) => P, continuous: Vector[C]): (util.Random, Genome) => Individual[P] =
    noisy.expression[Genome, Individual[P], P](
      values(_, continuous),
      buildIndividual[P])(fitness)

  def aggregate[P: Manifest](i: Individual[P], aggregation: Vector[P] => Vector[Double], pattern: Vector[Double] => Vector[Int], continuous: Vector[C]) =
    (
      scaleContinuousValues(continuousValues.get(i.genome), continuous),
      Individual.genome[P] composeLens discreteValues get i,
      aggregation(vectorPhenotype[P].get(i)),
      (vectorPhenotype[P].get _ andThen aggregation andThen pattern)(i),
      Individual.phenotypeHistory[P].get(i).size)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], aggregation: Vector[Double], pattern: Vector[Int], replications: Int)

  def result[P: Manifest](
    population: Vector[Individual[P]],
    aggregation: Vector[P] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    continuous: Vector[C]) =
    population.map {
      i =>
        val (c, d, f, p, r) = aggregate[P](i, aggregation, pattern, continuous)
        Result[P](c, d, f, p, r)
    }

  def result[P: Manifest](pse: NoisyPSE[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result(population, pse.aggregation, pse.pattern, pse.continuous)

  def run[T](rng: util.Random)(f: PSE.PSEImplicits => T): T = PSE.run(rng)(f)
  def run[T](state: EvolutionState[Map[Vector[Int], Int]])(f: PSE.PSEImplicits => T): T = PSE.run(state)(f)

  implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: HitMap: Generation, P: Manifest: CanBeNaN] = new Algorithm[NoisyPSE[P], M, Individual[P], Genome, EvolutionState[Map[Vector[Int], Int]]] {

    def initialPopulation(t: NoisyPSE[P]) =
      noisy.initialPopulation[M, Genome, Individual[P]](
        NoisyPSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyPSE.expression(t.phenotype, t.continuous))

    def step(t: NoisyPSE[P]) =
      noisy.step[M, Individual[P], Genome](
        NoisyPSE.adaptiveBreeding[M, P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete,
          t.pattern),
        NoisyPSE.expression(t.phenotype, t.continuous),
        NoisyPSE.elitism[M, P](
          t.pattern,
          t.aggregation,
          t.historySize,
          t.continuous))

    def state = NoisyPSE.state[M]

  }
}

case class NoisyPSE[P](
  lambda: Int,
  phenotype: (util.Random, Vector[Double], Vector[Int]) => P,
  pattern: Vector[Double] => Vector[Int],
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyPSEOperations {

  def adaptiveBreeding[M[_]: cats.Monad: Random: HitMap: Generation, I, G](
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
    operatorExploration: Double) = Breeding[M, I, G] { population =>
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

  def elitism[M[_]: cats.Monad: Random: Generation: HitMap, I, P: CanBeNaN](
    values: I => (Vector[Double], Vector[Int]),
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    mapped: monocle.Lens[I, Boolean],
    historyAge: monocle.Lens[I, Long],
    historySize: Int): Elitism[M, I] = Elitism[M, I] { population =>

    def unclone: UncloneStrategy[M, I] = (is: Vector[I]) => {
      def merged: M[I] = mergeHistories[M, I, P](historyAge, history)(historySize).apply(is)
      merged.map(mapped.set(false))
    }

    for {
      cloneRemoved <- applyCloneStrategy(
        values,
        unclone) apply filterNaN(population, history.get _ andThen aggregation)
      mappedPopulation <- addHits[M, I](history.get _ andThen aggregation andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(history.get _ andThen aggregation andThen pattern, maximiseO[M, I, Long](i => history.get(i).size, 1)) apply mappedPopulation
    } yield elite
  }

}
