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

import cats.implicits.*
import mgo.evolution.*
import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.tools.{CanBeNaN, ImplementEqualMethod}
import mgo.tools.execution.*
import monocle.*
import monocle.syntax.all.*

import scala.util.Random

object NoisyPSE {

  import CDGenome._

  type PSEState = EvolutionState[HitMap]


  type Individual[P] = CDGenome.NoisyIndividual.Individual[P]

  def buildIndividual[P: Manifest](genome: Genome, phenotype: P, generation: Long, initial: Boolean) = CDGenome.NoisyIndividual.buildIndividual[P](genome, phenotype, generation, initial)
  def vectorPhenotype[P: Manifest]: PLens[Individual[P], Individual[P], Vector[P], Vector[P]] = Focus[Individual[P]](_.phenotypeHistory) andThen arrayToVectorIso

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P: Manifest](
    lambda: Int,
    operatorExploration: Double,
    cloneProbability: Double,
    aggregation: Vector[P] => Vector[Double],
    continuous: Vector[C],
    discrete: Vector[D],
    pattern: Vector[Double] => Vector[Int],
    maxRareSample: Int,
    reject: Option[Genome => Boolean]): Breeding[PSEState, Individual[P], Genome] =
    NoisyPSEOperations.adaptiveBreeding[PSEState, Individual[P], Genome](
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      discrete,
      vectorPhenotype[P].get _ andThen aggregation andThen pattern,
      buildGenome(discrete),
      lambda,
      reject,
      operatorExploration,
      cloneProbability,
      Focus[EvolutionState[HitMap]](_.s),
      maxRareSample,
      (s, rng) => NoisyPSE.initialGenomes(s, continuous, discrete, reject, rng))

  def elitism[P: {CanBeNaN, Manifest}](
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[P] => Vector[Double],
    historySize: Int,
    continuous: Vector[C],
    discrete: Vector[D]): Elitism[PSEState, Individual[P]] =
    NoisyPSEOperations.elitism[PSEState, Individual[P], P](
      i => scaledValues(continuous, discrete)(i.genome),
      vectorPhenotype[P],
      aggregation,
      pattern,
      Focus[Individual[P]](_.historyAge),
      historySize,
      Focus[EvolutionState[HitMap]](_.s))

  def expression[P: Manifest](fitness: (util.Random, IArray[Double], IArray[Int]) => P, continuous: Vector[C], discrete: Vector[D]) =
    noisy.expression[Genome, Individual[P], P](
      scaledValues(continuous, discrete),
      buildIndividual[P])(fitness)

  def aggregate[P: Manifest](i: Individual[P], aggregation: Vector[P] => Vector[Double], pattern: Vector[Double] => Vector[Int], continuous: Vector[C], discrete: Vector[D]): (IArray[Double], IArray[Int], Vector[Double], Vector[Int], Int) =
    (
      scaleContinuousValues(continuousValues(continuous).get(i.genome), continuous),
      i.focus(_.genome) andThen discreteValues(discrete) get,
      aggregation(vectorPhenotype[P].get(i)),
      (vectorPhenotype[P].get _ andThen aggregation andThen pattern)(i),
      i.phenotypeHistory.size)

  case class Result[P](continuous: IArray[Double], discrete: IArray[Int], aggregation: Vector[Double], pattern: Vector[Int], replications: Int, individual: Individual[P])

  def result[P: Manifest](
    population: Vector[Individual[P]],
    aggregation: Vector[P] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    continuous: Vector[C],
    discrete: Vector[D]): Vector[Result[P]] =
    population.map: i =>
      val (c, d, f, p, r) = aggregate[P](i, aggregation, pattern, continuous, discrete)
      Result[P](c, d, f, p, r, i)

  def result[P: Manifest](pse: NoisyPSE[P], population: Vector[Individual[P]]): Vector[Result[P]] =
    result(population, pse.aggregation, pse.pattern, pse.continuous, pse.discrete)

  def reject[P](pse: NoisyPSE[P]): Option[Genome => Boolean] = NSGA2.reject(pse.reject, pse.continuous, pse.discrete)

  given isAlgorithm[P: {Manifest, CanBeNaN}]: Algorithm[NoisyPSE[P], Individual[P], Genome, PSEState] with {

    def initialState(t: NoisyPSE[P], rng: util.Random) = EvolutionState[HitMap](s = Map.empty)

    def initialPopulation(t: NoisyPSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        NoisyPSE.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        NoisyPSE.expression(t.phenotype, t.continuous, t.discrete),
        rng,
        parallel)

    def step(t: NoisyPSE[P]) =
      noisy.step[PSEState, Individual[P], Genome](
        NoisyPSE.adaptiveBreeding[P](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.continuous,
          t.discrete,
          t.pattern,
          t.maxRareSample,
          reject(t)),
        NoisyPSE.expression(t.phenotype, t.continuous, t.discrete),
        NoisyPSE.elitism[P](
          t.pattern,
          t.aggregation,
          t.historySize,
          t.continuous,
          t.discrete),
        Focus[PSEState](_.generation),
        Focus[PSEState](_.evaluated))

  }
}

case class NoisyPSE[P](
  lambda: Int,
  phenotype: (util.Random, IArray[Double], IArray[Int]) => P,
  pattern: Vector[Double] => Vector[Int],
  aggregation: Vector[P] => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  maxRareSample: Int = 10,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object NoisyPSEOperations:

  def adaptiveBreeding[S, I, G](
    genome: I => G,
    continuousValues: G => IArray[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => IArray[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    pattern: I => Vector[Int],
    buildGenome: (IArray[Double], Option[Int], IArray[Int], Option[Int]) => G,
    lambda: Int,
    reject: Option[G => Boolean],
    cloneProbability: Double,
    operatorExploration: Double,
    hitmap: monocle.Lens[S, HitMap],
    maxRareSample: Int,
    randomGenomes: (Int, Random) => Vector[G]): Breeding[S, I, G] =
    (s, population, rng) =>
      val gs =
        PSEOperations.adaptiveBreeding[S, I, G](
          genome,
          continuousValues,
          continuousOperator,
          discreteValues,
          discreteOperator,
          discrete,
          pattern,
          buildGenome,
          lambda,
          reject,
          operatorExploration,
          hitmap,
          maxRareSample,
          randomGenomes)(s, population, rng)
      clonesReplace[S, I, G](cloneProbability, population, genome, randomSelection)(s, gs, rng)

  def elitism[S, I, P: CanBeNaN](
    values: I => (IArray[Double], IArray[Int]),
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    historyAge: monocle.Lens[I, Long],
    historySize: Int,
    hitmap: monocle.Lens[S, HitMap]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedPattern = mgo.tools.memoize(history.get _ andThen aggregation andThen pattern)
      val eqm = summon[ImplementEqualMethod[(IArray[Double], IArray[Int])]]
      val candidateValues = candidates.map(values andThen eqm.apply).toSet
      val merged = filterNaN(mergeHistories(values, history, historyAge, historySize).apply(population, candidates), history.get _ andThen aggregation)

      def newHits =
        merged.flatMap: i =>
          if candidateValues.contains(eqm(values(i))) then Some(i) else None

      val hm2 = addHits[I](memoizedPattern, newHits, hitmap.get(s))
      val elite = keepNiches[I, Vector[Int]](memoizedPattern, maximiseO(i => history.get(i).size, 1)) apply merged
      (hitmap.set(hm2)(s), elite)


