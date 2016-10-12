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
package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo
import fr.iscpif.mgo._
import elitism._
import breeding._
import fr.iscpif.mgo.tools.CanBeNaN
import contexts._
import GenomeVectorDouble._

import monocle.macros._
import scala.util.Random

import scalaz._
import Scalaz._

object noisypse {

  implicit def hitMapper = pse.hitMapper

  object NoisyPSE {

    implicit def isAlgorithme = new Algorithm[NoisyPSE, EvolutionState[pse.HitMap, ?], Individual, Genome, EvolutionData[pse.HitMap]] {
      def initialState(t: NoisyPSE, rng: Random) = EvolutionData[pse.HitMap](random = rng, s = Map())

      def initialPopulation(t: NoisyPSE) =
        stochasticInitialPopulation[EvolutionState[pse.HitMap, ?], Genome, Individual](
          noisypse.initialGenomes(t.lambda, t.genomeSize),
          noisypse.expression(t.phenotype))

      def step(t: NoisyPSE) =
        noisypseOperations.step[EvolutionState[pse.HitMap, ?], Individual, Genome](
          noisypse.breeding(
            lambda = t.lambda,
            pattern = t.pattern,
            cloneProbability = t.cloneProbability,
            operatorExploration = t.operatorExploration,
            aggregation = t.aggregation),
          noisypse.expression(t.phenotype),
          noisypse.elitism(
            pattern = t.pattern,
            historySize = t.historySize,
            aggregation = t.aggregation)
        )

      def run[A](x: EvolutionState[pse.HitMap, A], s: EvolutionData[pse.HitMap]): (EvolutionData[pse.HitMap], A) = mgo.unwrap(x, s)
    }

  }

  case class NoisyPSE(
    lambda: Int,
    phenotype: (Random, Vector[Double]) => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genomeSize: Int,
    historySize: Int = 100,
    cloneProbability: Double = 0.2,
    operatorExploration: Double = 0.1)

  @Lenses case class Genome(values: Array[Double], operator: Maybe[Int])
  @Lenses case class Individual(
    genome: Genome,
    historyAge: Long,
    phenotypeHistory: Array[Array[Double]],
    age: Long,
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildGenome(values: Vector[Double], operator: Maybe[Int]) = Genome(values.toArray, operator)
  def buildIndividual(genome: Genome, phenotype: Vector[Double]) = Individual(genome, 1, Array(phenotype.toArray), 0)

  def vectorValues = Genome.values composeLens arrayToVectorLens
  def vectorPhenotype = Individual.phenotypeHistory composeLens array2ToVectorLens

  def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[pse.HitMap, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[pse.HitMap, ?], Genome](buildGenome)(mu, genomeSize)

  def breeding(
    lambda: Int,
    aggregation: Vector[Vector[Double]] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    cloneProbability: Double,
    operatorExploration: Double) =
    noisypseOperations.breeding[EvolutionState[pse.HitMap, ?], Individual, Genome, Vector[Double]](
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      vectorPhenotype.get _ andThen aggregation andThen pattern,
      buildGenome
    )(lambda, cloneProbability, operatorExploration)

  def elitism(pattern: Vector[Double] => Vector[Int], aggregation: Vector[Vector[Double]] => Vector[Double], historySize: Int) =
    noisypseOperations.elitism[EvolutionState[pse.HitMap, ?], Individual, Vector[Double]](
      (Individual.genome composeLens vectorValues).get,
      vectorPhenotype,
      aggregation,
      pattern,
      Individual.age,
      Individual.mapped,
      Individual.historyAge
    )(historySize)

  def expression(phenotype: (Random, Vector[Double]) => Vector[Double]) =
    noisypseOperations.expression[Genome, Individual, Vector[Double]](
      vectorValues.get,
      buildIndividual)(phenotype)

  case class OpenMOLE(
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genomeSize: Int,
    historySize: Int,
    cloneProbability: Double,
    operatorExploration: Double)

  object OpenMOLE {
    implicit def integration = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] with openmole.Stochastic {
      type M[A] = EvolutionState[pse.HitMap, A]
      type G = Genome
      type I = Individual
      type S = EvolutionData[pse.HitMap]

      def iManifest = implicitly
      def gManifest = implicitly
      def sManifest = implicitly
      def mMonad = implicitly
      def mGenerational = implicitly
      def mStartTime = implicitly

      def operations(om: OpenMOLE) = new Ops {
        def randomLens = GenLens[S](_.random)
        def startTimeLens = GenLens[S](_.startTime)
        def generation(s: EvolutionData[pse.HitMap]) = s.generation
        def values(genome: G) = vectorValues.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = om.aggregation(vectorPhenotype.get(individual))
        def buildIndividual(genome: G, phenotype: Vector[Double]) = noisypse.buildIndividual(genome, phenotype)
        def initialState(rng: Random) = EvolutionData[pse.HitMap](random = rng, s = Map())
        def initialGenomes(n: Int) = noisypse.initialGenomes(n, om.genomeSize)
        def breeding(n: Int) =
          noisypse.breeding(
            lambda = n,
            operatorExploration = om.operatorExploration,
            cloneProbability = om.cloneProbability,
            aggregation = om.aggregation,
            pattern = om.pattern
          )

        def elitism =
          noisypse.elitism(
            pattern = om.pattern,
            historySize = om.historySize,
            aggregation = om.aggregation)

        def migrateToIsland(population: Vector[I]) =
          population.map(Individual.foundedIsland.set(true)).map(Individual.historyAge.set(0))

        def migrateFromIsland(population: Vector[I]) =
          population.filter(_.historyAge != 0).map {
            i =>
              val i1 = Individual.phenotypeHistory.modify(_.take(math.min(i.historyAge, om.historySize).toInt))(i)
              if (Individual.foundedIsland.get(i1))
                (Individual.mapped.set(true) andThen Individual.foundedIsland.set(false))(i1)
              else Individual.mapped.set(false)(i1)
          }

      }

      def unwrap[A](x: EvolutionState[pse.HitMap, A], s: S): (EvolutionData[pse.HitMap], A) = mgo.unwrap[pse.HitMap, A](x, s)
      def samples(i: I): Long = i.phenotypeHistory.size
    }
  }

}

object noisypseOperations {

  def breeding[M[_]: Monad: RandomGen: Generational, I, G, P](
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Maybe[Int],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Maybe[Int]) => G)(
      lambda: Int,
      cloneProbability: Double,
      operatorExploration: Double)(implicit MH: HitMapper[M, Vector[Int]]): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      gs <- pseOperations.breeding[M, I, G](
        genome = genome,
        genomeValues = genomeValues,
        pattern = pattern,
        genomeOperator = genomeOperator,
        buildGenome = buildGenome
      )(lambda, operatorExploration) andThen clonesReplace(cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: Monad: RandomGen: Generational, I, P: CanBeNaN](
    values: I => Vector[Double],
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => P,
    pattern: P => Vector[Int],
    age: monocle.Lens[I, Long],
    mapped: monocle.Lens[I, Boolean],
    historyAge: monocle.Lens[I, Long])(historySize: Int)(implicit MH: HitMapper[M, Vector[Int]]): Elitism[M, I] =
    addHits[M, I, Vector[Int]](history.get _ andThen aggregation andThen pattern, mapped) andThen
      applyCloneStrategy(values, mergeHistories[M, I, P](historyAge, history)(historySize)) andThen
      filterNaN(history.get _ andThen aggregation) andThen
      keepNiches(
        niche = history.get _ andThen aggregation andThen pattern,
        objective = randomO[M, I](1)
      ) andThen incrementGeneration(age)

  def expression[G, I, P](
    values: G => Vector[Double],
    builder: (G, P) => I)(phenotype: (Random, Vector[Double]) => P): Expression[(Random, G), I] = {
    case (rg, g) => builder(g, phenotype(rg, values(g)))
  }

  def step[M[_]: Monad: RandomGen: Generational: ParallelRandomGen, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)

}