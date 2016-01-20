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

import fr.iscpif.mgo._
import elitism._
import breeding._
import fr.iscpif.mgo.tools.CanBeNaN
import niche._
import contexts._
import ranking._
import GenomeVectorDouble._

import monocle.macros._
import scala.util.Random
import scalaz._
import Scalaz._

object noisypse {
  import default._
  implicit def hitMapper = pse.hitMapper

  case class NoisyPSE(
      lambda: Int,
      phenotype: (Random, Vector[Double]) => Vector[Double],
      pattern: Vector[Double] => Vector[Int],
      aggregation: Vector[Vector[Double]] => Vector[Double],
      genomeSize: Int,
      historySize: Int = 100,
      cloneProbability: Double = 0.2,
      operatorExploration: Double = 0.1) extends Algorithm[EvolutionState[pse.HitMap, ?], Individual, Genome, EvolutionData[pse.HitMap]] {
    def initialState(rng: Random) = EvolutionData[pse.HitMap](random = rng, s = Map())
    def initialGenomes = noisypse.initialGenomes(lambda, genomeSize)
    def breeding = noisypse.breeding(
      lambda = lambda,
      pattern = pattern,
      cloneProbability = cloneProbability,
      operatorExploration = operatorExploration,
      aggregation = aggregation)

    def expression = noisypse.expression(phenotype)

    def elitism = noisypse.elitism(pattern = pattern, historySize = historySize, aggregation = aggregation)

    def step =
      noisypseOperations.step[EvolutionState[pse.HitMap, ?], Individual, Genome](
        breeding,
        expression,
        elitism)

    def run[A](x: EvolutionState[pse.HitMap, A], s: EvolutionData[pse.HitMap]): (EvolutionData[pse.HitMap], A) = default.unwrap(x, s)
  }

  type V = Vector[Double]

  @Lenses case class Genome(values: V, operator: Maybe[Int])
  @Lenses case class Individual(genome: Genome, historyAge: Long, phenotypeHistory: Vector[Vector[Double]], age: Long)

  def buildIndividual(genome: Genome, phenotype: Vector[Double]) = Individual(genome, 1, Vector(phenotype), 0)

  def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[pse.HitMap, Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[EvolutionState[pse.HitMap, ?], Genome](Genome.apply)(mu, genomeSize)

  def breeding(
    lambda: Int,
    aggregation: Vector[Vector[Double]] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    cloneProbability: Double,
    operatorExploration: Double) =
    noisypseOperations.breeding[EvolutionState[pse.HitMap, ?], Individual, Genome, Vector[Double]](
      Individual.genome.get,
      Genome.values.get,
      Genome.operator.get,
      Individual.phenotypeHistory.get _ andThen aggregation andThen pattern,
      Genome.apply
    )(lambda, cloneProbability, operatorExploration)

  def elitism(pattern: Vector[Double] => Vector[Int], aggregation: Vector[Vector[Double]] => Vector[Double], historySize: Int) =
    noisypseOperations.elitism[EvolutionState[pse.HitMap, ?], Individual, Vector[Double]](
      (Individual.genome composeLens Genome.values).get,
      Individual.phenotypeHistory,
      aggregation,
      pattern,
      Individual.age,
      Individual.historyAge
    )(historySize)

  def expression(phenotype: (Random, Vector[Double]) => Vector[Double]) =
    noisypseOperations.expression[Genome, Individual, Vector[Double]](
      Genome.values.get,
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
        def values(genome: G) = Genome.values.get(genome)
        def genome(i: I) = Individual.genome.get(i)
        def phenotype(individual: I): Vector[Double] = om.aggregation(Individual.phenotypeHistory.get(individual))
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

        def migrateToIsland(i: Individual): Individual = i.copy(historyAge = 0)
        def migrateFromIsland(i: I) = Individual.phenotypeHistory.modify(_.take(math.min(i.historyAge, om.historySize).toInt))(i)
      }

      def unwrap[A](x: EvolutionState[pse.HitMap, A], s: S): (EvolutionData[pse.HitMap], A) = default.unwrap[pse.HitMap, A](x, s)
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
    historyAge: monocle.Lens[I, Long])(historySize: Int)(implicit MH: HitMapper[M, Vector[Int]]): Elitism[M, I] =
    addHits[M, I, Vector[Int]](history.get _ andThen aggregation andThen pattern, age.get) andThen
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