/*
 * Copyright (C) 15/12/2015 Guillaume Chérel
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
package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo.algorithm.GenomeVectorDouble._
import monocle.macros.{ Lenses, GenLens }

import scala.util.Random
import scalaz._
import Scalaz._

import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo._
import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.contexts._
import fr.iscpif.mgo.contexts.default._

import scala.language.higherKinds

object noisynsga2 {

  @Lenses case class Genome(values: Array[Double], operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, historyAge: Long, fitnessHistory: Array[Array[Double]], age: Long)

  def buildGenome(values: Vector[Double], operator: Maybe[Int]) = Genome(values.toArray, operator)
  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, 1, Array(f.toArray), 0)

  def vectorFitness = Individual.fitnessHistory composeLens array2ToVectorLens
  def vectorValues = Genome.values composeLens arrayToVectorLens

  def initialGenomes(lambda: Int, genomeSize: Int): EvolutionState[Unit, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[Unit, ?], Genome](buildGenome)(lambda, genomeSize)

  def breeding(lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double]): Breeding[EvolutionState[Unit, ?], Individual, Genome] =
    noisynsga2Operations.breeding[EvolutionState[Unit, ?], Individual, Genome](
      vectorFitness.get,
      aggregation,
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      buildGenome)(lambda, operatorExploration, cloneProbability)

  def expression(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, Genome), Individual] =
    noisynsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism(mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double]): Elitism[EvolutionState[Unit, ?], Individual] =
    noisynsga2Operations.elitism[EvolutionState[Unit, ?], Individual](
      vectorFitness,
      aggregation,
      (Individual.genome composeLens vectorValues).get,
      Individual.age,
      Individual.historyAge
    )(mu, historySize)

  case class NoisyNSGA2(
      mu: Int,
      lambda: Int,
      fitness: (Random, Vector[Double]) => Vector[Double],
      aggregation: Vector[Vector[Double]] => Vector[Double],
      genomeSize: Int,
      historySize: Int = 100,
      cloneProbability: Double = 0.2,
      operatorExploration: Double = 0.1) extends Algorithm[EvolutionState[Unit, ?], Individual, Genome, EvolutionData[Unit]] {
    def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
    def initialGenomes: EvolutionState[Unit, Vector[Genome]] = noisynsga2.initialGenomes(lambda, genomeSize)

    def breeding: Breeding[EvolutionState[Unit, ?], Individual, Genome] = noisynsga2.breeding(lambda, operatorExploration, cloneProbability, aggregation)

    def expression: Expression[(Random, Genome), Individual] = noisynsga2.expression(fitness)

    def elitism: Elitism[EvolutionState[Unit, ?], Individual] = noisynsga2.elitism(mu, historySize, aggregation)

    def step: Kleisli[EvolutionState[Unit, ?], Vector[Individual], Vector[Individual]] =
      noisynsga2Operations.step[EvolutionState[Unit, ?], Individual, Genome](
        breeding,
        expression,
        elitism)

    def run[A](x: EvolutionState[Unit, A], s: EvolutionData[Unit]): (EvolutionData[Unit], A) = default.unwrap(x, s)
  }

  case class OpenMOLE(
    mu: Int,
    operatorExploration: Double,
    genomeSize: Int,
    historySize: Int,
    cloneProbability: Double,
    aggregation: Vector[Vector[Double]] => Vector[Double])

  object OpenMOLE {
    implicit def integration = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] with openmole.Stochastic {
      type M[A] = EvolutionState[Unit, A]
      type G = Genome
      type I = Individual
      type S = EvolutionData[Unit]

      def iManifest = implicitly
      def gManifest = implicitly
      def sManifest = implicitly
      def mMonad = implicitly
      def mGenerational = implicitly
      def mStartTime = implicitly

      def operations(om: OpenMOLE) = new Ops {
        def randomLens = GenLens[S](_.random)
        def startTimeLens = GenLens[S](_.startTime)
        def generation(s: EvolutionData[Unit]) = s.generation
        def values(genome: G) = vectorValues.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = om.aggregation(vectorFitness.get(individual))
        def buildIndividual(genome: G, phenotype: Vector[Double]) = noisynsga2.buildIndividual(genome, phenotype)
        def initialState(rng: Random) = EvolutionData[Unit](random = rng, s = ())
        def initialGenomes(n: Int): EvolutionState[Unit, Vector[Genome]] = noisynsga2.initialGenomes(n, om.genomeSize)
        def breeding(n: Int): Breeding[EvolutionState[Unit, ?], Individual, Genome] = noisynsga2.breeding(n, om.operatorExploration, om.cloneProbability, om.aggregation)
        def elitism: Elitism[EvolutionState[Unit, ?], Individual] = noisynsga2.elitism(om.mu, om.historySize, om.aggregation)

        def migrateToIsland(population: Vector[I]) = population.map(_.copy(historyAge = 0))
        def migrateFromIsland(population: Vector[I]) =
          population.filter(_.historyAge != 0).map {
            i => Individual.fitnessHistory.modify(_.take(math.min(i.historyAge, om.historySize).toInt))(i)
          }
      }

      def unwrap[A](x: EvolutionState[Unit, A], s: S): (EvolutionData[Unit], A) = default.unwrap[Unit, A](x, s)
      def samples(i: I): Long = i.fitnessHistory.size
    }
  }

}

object noisynsga2Operations {

  def aggregated[I](fitness: I => Vector[Vector[Double]], aggregation: Vector[Vector[Double]] => Vector[Double])(i: I): Vector[Double] =
    aggregation(fitness(i)) ++ Vector(1.0 / fitness(i).size.toDouble)

  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
    history: I => Vector[Vector[Double]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      gs <- nsga2Operations.breeding[M, I, G](
        aggregated(history, aggregation),
        genome,
        genomeValues,
        genomeOperator,
        buildGenome
      )(lambda, operatorExploration) andThen clonesReplace(cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: Monad: RandomGen: Generational, I](
    history: monocle.Lens[I, Vector[Vector[Double]]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long])(mu: Int, historySize: Int): Elitism[M, I] =
    applyCloneStrategy(values, mergeHistories[M, I, Vector[Double]](historyAge, history)(historySize)) andThen
      filterNaN(aggregated(history.get, aggregation)) andThen
      keepHighestRanked(paretoRankingMinAndCrowdingDiversity[M, I](aggregated(history.get, aggregation)), mu) andThen
      incrementGeneration(age)

  def expression[G, I](
    values: G => Vector[Double],
    builder: (G, Vector[Double]) => I)(fitness: (Random, Vector[Double]) => Vector[Double]): Expression[(Random, G), I] = {
    case (rg, g) => builder(g, fitness(rg, values(g)))
  }

  def step[M[_]: Monad: RandomGen: Generational: ParallelRandomGen, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)
}