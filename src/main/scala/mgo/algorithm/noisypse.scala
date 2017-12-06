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

object noisypse extends niche.Imports {

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: mgo.contexts.HitMap[M, Vector[Int]]) = pse.state[M]

  object NoisyPSE {

    import pse.{ PSE }
    def run[T](rng: util.Random)(f: PSE.PSEImplicits => T): T = PSE.run(rng)(f)
    def run[T](state: EvolutionState[Map[Vector[Int], Int]])(f: PSE.PSEImplicits => T): T = PSE.run(state)(f)

    implicit def isAlgorithm[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: HitMap[M, Vector[Int]]) = new Algorithm[NoisyPSE, M, Individual, Genome, EvolutionState[Map[Vector[Int], Int]]] {

      def initialPopulation(t: NoisyPSE) =
        stochasticInitialPopulation[M, Genome, Individual](
          noisypse.initialGenomes[M](t.lambda, t.genomeSize),
          noisypse.expression(t.phenotype))

      def step(t: NoisyPSE) =
        noisypseOperations.step[M, Individual, Genome](
          noisypse.breeding[M](
            lambda = t.lambda,
            pattern = t.pattern,
            cloneProbability = t.cloneProbability,
            operatorExploration = t.operatorExploration,
            aggregation = t.aggregation),
          noisypse.expression(t.phenotype),
          noisypse.elitism[M](
            pattern = t.pattern,
            historySize = t.historySize,
            aggregation = t.aggregation))

      def state = noisypse.state[M]

    }

  }

  case class NoisyPSE(
    lambda: Int,
    phenotype: (util.Random, Vector[Double]) => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genomeSize: Int,
    historySize: Int = 100,
    cloneProbability: Double = 0.2,
    operatorExploration: Double = 0.1)

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(
    genome: Genome,
    historyAge: Long,
    phenotypeHistory: Array[Array[Double]],
    age: Long,
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)
  def buildIndividual(genome: Genome, phenotype: Vector[Double]) = Individual(genome, 1, Array(phenotype.toArray), 0)

  def vectorValues = Genome.values composeLens arrayToVectorLens
  def vectorPhenotype = Individual.phenotypeHistory composeLens array2ToVectorLens

  def initialGenomes[M[_]: cats.Monad: Random](mu: Int, genomeSize: Int) =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(mu, genomeSize)

  def breeding[M[_]: cats.Monad: Random: Generation](
    lambda: Int,
    aggregation: Vector[Vector[Double]] => Vector[Double],
    pattern: Vector[Double] => Vector[Int],
    cloneProbability: Double,
    operatorExploration: Double)(implicit MH: HitMap[M, Vector[Int]]) =
    noisypseOperations.breeding[M, Individual, Genome](
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      vectorPhenotype.get _ andThen aggregation andThen pattern,
      buildGenome)(lambda, cloneProbability, operatorExploration)

  def elitism[M[_]: cats.Monad: Random: Generation](pattern: Vector[Double] => Vector[Int], aggregation: Vector[Vector[Double]] => Vector[Double], historySize: Int)(implicit MH: HitMap[M, Vector[Int]]) =
    noisypseOperations.elitism[M, Individual, Vector[Double]](
      (Individual.genome composeLens vectorValues).get,
      vectorPhenotype,
      aggregation,
      pattern,
      Individual.age,
      Individual.mapped,
      Individual.historyAge)(historySize)

  def expression(phenotype: (util.Random, Vector[Double]) => Vector[Double]) =
    noisypseOperations.expression[Genome, Individual, Vector[Double]](
      vectorValues.get,
      buildIndividual)(phenotype)

}

object noisypseOperations {

  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
    lambda: Int,
    cloneProbability: Double,
    operatorExploration: Double)(implicit MH: HitMap[M, Vector[Int]]): Breeding[M, I, G] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      gs <- pseOperations.breeding[M, I, G](
        genome = genome,
        genomeValues = genomeValues,
        pattern = pattern,
        genomeOperator = genomeOperator,
        buildGenome = buildGenome)(lambda, operatorExploration) andThen clonesReplace[M, I, G](cloneProbability, population, genome)
    } yield gs

  def elitism[M[_]: cats.Monad: Random: Generation, I, P: CanBeNaN](
    values: I => Vector[Double],
    history: monocle.Lens[I, Vector[P]],
    aggregation: Vector[P] => P,
    pattern: P => Vector[Int],
    age: monocle.Lens[I, Long],
    mapped: monocle.Lens[I, Boolean],
    historyAge: monocle.Lens[I, Long])(historySize: Int)(implicit MH: HitMap[M, Vector[Int]]): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, P](historyAge, history)(historySize)) apply filterNaN(population, values)
      mappedPopulation <- addHits[M, I, Vector[Int]](history.get _ andThen aggregation andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(history.get _ andThen aggregation andThen pattern, maximiseO[M, I, Long](i => history.get(i).size, 1)) apply mappedPopulation
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def expression[G, I, P](
    values: G => Vector[Double],
    builder: (G, P) => I)(phenotype: (util.Random, Vector[Double]) => P): Expression[(util.Random, G), I] = {
    case (rg, g) => builder(g, phenotype(rg, values(g)))
  }

  def step[M[_]: cats.Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(util.Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)

}