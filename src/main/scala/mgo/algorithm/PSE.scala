/*
 * Copyright (C) 16/12/2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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
package mgo.algorithm

import mgo._
import mgo.breeding._
import mgo.contexts._
import mgo.ranking._
import mgo.elitism._
import mgo.tools._
import mgo.niche._
import GenomeVectorDouble._
import monocle.macros.{ GenLens, Lenses }

import shapeless._
import scala.language.higherKinds
import cats._
import cats.implicits._
import freedsl.dsl
import freedsl.io.IOInterpreter
import freedsl.random.RandomInterpreter
import freedsl.system.SystemInterpreter
import freedsl.tool._
import freestyle.tagless._

object PSE extends niche.Imports {

  type HitMapM[M[_]] = HitMap[M, Vector[Int]]
  type HitMapState = Map[Vector[Int], Int]

  import CDGenome._

  case class Result(continuous: Vector[Double], discrete: Vector[Int], pattern: Vector[Int], phenotype: Vector[Double])

  def result(population: Vector[Individual], continuous: Vector[C], pattern: Vector[Double] => Vector[Int]) =
    population.map { i =>
      Result(
        scaleContinuousValues(continuousValues.get(i.genome), continuous),
        Individual.genome composeLens discreteValues get i,
        pattern(i.phenotype.toVector),
        i.phenotype.toVector)
    }

  def result(pse: PSE, population: Vector[Individual]): Vector[Result] =
    result(population, pse.continuous, pse.pattern)

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: HitMapM[M]) = for {
    map <- hitmap.get
    s <- mgo.algorithm.state[M, Map[Vector[Int], Int]](map)
  } yield s

  @Lenses case class Individual(
    genome: Genome,
    phenotype: Array[Double],
    age: Long,
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)
  def vectorPhenotype = Individual.phenotype composeLens arrayToVectorLens

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad: HitMapM](
    lambda: Int,
    operatorExploration: Double,
    discrete: Vector[D],
    pattern: Vector[Double] => Vector[Int]): Breeding[M, Individual, Genome] =
    PSEOperations.adaptiveBreeding[M, Individual, Genome](
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      vectorPhenotype.get _ andThen pattern,
      buildGenome,
      lambda,
      operatorExploration)

  def elitism[M[_]: cats.Monad: StartTime: Random: Generation](pattern: Vector[Double] => Vector[Int], continuous: Vector[C])(implicit hm: HitMap[M, Vector[Int]]) =
    PSEOperations.elitism[M, Individual, Vector[Double]](
      i => values(Individual.genome.get(i), continuous),
      vectorPhenotype.get,
      pattern,
      Individual.age,
      Individual.mapped)

  def expression(phenotype: (Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): Genome => Individual =
    deterministic.expression[Genome, Individual](
      values(_, continuous),
      buildIndividual,
      phenotype)

  object PSEImplicits {
    def apply(state: EvolutionState[Map[Vector[Int], Int]]): PSEImplicits =
      PSEImplicits()(GenerationInterpreter(state.generation), RandomInterpreter(state.random), StartTimeInterpreter(state.startTime), IOInterpreter(), VectorHitMapInterpreter(state.s), SystemInterpreter())
  }

  case class PSEImplicits(implicit generation: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, hitMapInterpreter: VectorHitMapInterpreter, systemInterpreter: SystemInterpreter)

  def run[T](rng: util.Random)(f: PSEImplicits => T): T = {
    val state = EvolutionState[Map[Vector[Int], Int]](random = rng, s = Map.empty)
    run(state)(f)
  }

  def run[T, S](state: EvolutionState[HitMapState])(f: PSEImplicits => T): T = f(PSEImplicits(state))

  implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: Generation: HitMapM]: Algorithm[PSE, M, Individual, Genome, EvolutionState[Map[Vector[Int], Int]]] = new Algorithm[PSE, M, Individual, Genome, EvolutionState[Map[Vector[Int], Int]]] {

    // def initialState(t: PSE, rng: util.Random) = EvolutionState[HitMap](random = rng, s = Map.empty)
    override def initialPopulation(t: PSE) =
      deterministic.initialPopulation[M, Genome, Individual](
        PSE.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        PSE.expression(t.phenotype, t.continuous))

    def step(t: PSE) =
      deterministic.step[M, Individual, Genome](
        PSE.adaptiveBreeding[M](t.lambda, t.operatorExploration, t.discrete, t.pattern),
        PSE.expression(t.phenotype, t.continuous),
        PSE.elitism(t.pattern, t.continuous))

    def state = PSE.state[M]

  }

}

case class PSE(
  lambda: Int,
  phenotype: (Vector[Double], Vector[Int]) => Vector[Double],
  pattern: Vector[Double] => Vector[Int],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1)

object PSEOperations {

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation: PSE.HitMapM, I, G](
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    lambda: Int,
    operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>

    for {
      ranks <- reversedRanking(hitCountRanking[M, I, Vector[Int]](pattern)) apply population
      continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
      discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)
      breeding = applyDynamicOperators[M, I, G](
        tournament(ranks, logOfPopulationSize),
        genome andThen continuousValues,
        genome andThen discreteValues,
        continuousOperatorStatistics,
        discreteOperatorStatistics,
        discrete,
        operatorExploration,
        buildGenome) apply population
      offspring <- breeding repeat ((lambda + 1) / 2)
      sizedOffspringGenomes <- randomTake[M, G](offspring.flatMap { case (g1, g2) => Vector(g1, g2) }, lambda)
    } yield sizedOffspringGenomes
  }

  def elitism[M[_]: cats.Monad: Random: Generation: PSE.HitMapM, I, P: CanBeNaN](
    values: I => (Vector[Double], Vector[Int]),
    phenotype: I => P,
    pattern: P => Vector[Int],
    age: monocle.Lens[I, Long],
    mapped: monocle.Lens[I, Boolean]): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, phenotype)
      mappedPopulation <- addHits[M, I, Vector[Int]](phenotype andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(phenotype andThen pattern, randomO[M, I](1)) apply mappedPopulation
    } yield elite
  } andThen incrementAge[M, I](age)

}
