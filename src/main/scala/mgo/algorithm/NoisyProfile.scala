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
package mgo.algorithm

import mgo._
import mgo.breeding._
import mgo.elitism._
import mgo.diversity._
import monocle.macros.{ GenLens, Lenses }
import mgo.niche._
import mgo.contexts._
import mgo.ranking._
import GenomeVectorDouble._
import cats.data._
import cats.implicits._
import tools.Lazy
import freedsl.tool._
import mgo.niche

import scala.language.higherKinds

object noisyprofile extends niche.Imports {

  def genomeProfile(x: Int, nX: Int): Niche[Individual, Int] =
    genomeProfile[Individual]((Individual.genome composeLens vectorValues).get _, x, nX)

  def result(population: Vector[Individual], aggregation: Vector[Double] => Double, scaling: Vector[Double] => Vector[Double], niche: Niche[Individual, Int]) =
    profile(population, niche).map { i =>
      scaling(i.genome.values.toVector) -> aggregation(i.fitnessHistory.toVector)
    }

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(genome: Genome, historyAge: Long, fitnessHistory: Array[Double], age: Long)

  def vectorValues = Genome.values composeLens arrayToVectorLens
  def vectorFitness = Individual.fitnessHistory composeLens arrayToVectorLens

  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)
  def buildIndividual(g: Genome, f: Double) = Individual(g, 1, Array(f), 0)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, genomeSize: Int): M[Vector[Genome]] =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(lambda, genomeSize)

  def breeding[M[_]: cats.Monad: Random: Generation](lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Double] => Double): Breeding[M, Individual, Genome] =
    noisyprofileOperations.breeding[M, Individual, Genome](
      vectorFitness.get, aggregation, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome
    )(lambda = lambda, niche = niche, operatorExploration = operatorExploration, cloneProbability = cloneProbability)

  def expression(fitness: (util.Random, Vector[Double]) => Double): Expression[(util.Random, Genome), Individual] =
    noisyprofileOperations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism[M[_]: cats.Monad: Random: Generation](muByNiche: Int, niche: Niche[Individual, Int], historySize: Int, aggregation: Vector[Double] => Double): Elitism[M, Individual] =
    noisyprofileOperations.elitism[M, Individual](
      history = vectorFitness,
      aggregation = aggregation,
      values = (Individual.genome composeLens vectorValues).get,
      age = Individual.age,
      historyAge = Individual.historyAge
    )(muByNiche, niche, historySize)

  def profile(population: Vector[Individual], niche: Niche[Individual, Int]) =
    noisyprofileOperations.profile(population, niche, Individual.historyAge.get)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NoisyProfile {

    import contexts.run
    def apply[T](rng: util.Random)(f: run.Implicits => T): T = run(rng)(f)
    def apply[T](state: EvolutionState[Unit])(f: run.Implicits => T): T = run(state)(f)

    implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime]: Algorithm[NoisyProfile, M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[NoisyProfile, M, Individual, Genome, EvolutionState[Unit]] {
      def initialPopulation(t: NoisyProfile) =
        stochasticInitialPopulation[M, Genome, Individual](
          noisyprofile.initialGenomes[M](t.lambda, t.genomeSize),
          noisyprofile.expression(t.fitness))

      def step(t: NoisyProfile) = {
        def breeding =
          noisyprofile.breeding[M](
            lambda = t.lambda,
            niche = t.niche,
            operatorExploration = t.operatorExploration,
            cloneProbability = t.cloneProbability,
            aggregation = t.aggregation
          )

        def elitism =
          noisyprofile.elitism[M](
            muByNiche = t.muByNiche,
            niche = t.niche,
            historySize = t.historySize,
            aggregation = t.aggregation
          )

        noisyprofileOperations.step[M, Individual, Genome](
          breeding,
          noisyprofile.expression(t.fitness),
          elitism)
      }

      def state = noisyprofile.state[M]
    }

  }

  case class NoisyProfile(
    muByNiche: Int,
    lambda: Int,
    fitness: (util.Random, Vector[Double]) => Double,
    aggregation: Vector[Double] => Double,
    niche: Niche[Individual, Int],
    genomeSize: Int,
    historySize: Int = 100,
    cloneProbability: Double = 0.2,
    operatorExploration: Double = 0.1)

}

object noisyprofileOperations {

  def aggregatedFitness[I](fitness: I => Vector[Double], aggregation: Vector[Double] => Double)(i: I): Vector[Double] =
    Vector(aggregation(fitness(i)), 1.0 / fitness(i).size.toDouble)

  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
    history: I => Vector[Double],
    aggregation: Vector[Double] => Double,
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
      lambda: Int,
      niche: Niche[I, Int],
      operatorExploration: Double,
      cloneProbability: Double): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history, aggregation)) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperator[M, I](
        tournament[M, I, (Lazy[Int], Lazy[Double])](population, ranks, rounds = size => math.round(math.log10(size).toInt)),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration
      )
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case ((o1, o2), op) =>
          def gv1 = o1.map(tools.math.clamp(_))
          def gv2 = o2.map(tools.math.clamp(_))
          Vector(buildGenome(gv1, Some(op)), buildGenome(gv2, Some(op)))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
      withClones <- clonesReplace[M, I, G](cloneProbability, population, genome) apply sizedOffspringGenomes
    } yield withClones
  }

  def elitism[M[_]: cats.Monad: Random: Generation, I](
    history: monocle.Lens[I, Vector[Double]],
    aggregation: Vector[Double] => Double,
    values: I => Vector[Double],
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long])(muByNiche: Int, niche: Niche[I, Int], historySize: Int): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Double](historyAge, history)(historySize)) apply filterNaN(population, aggregatedFitness(history.get, aggregation))
      elite <- keepNiches[M, I, Int](
        niche = niche,
        objective =
          for {
            ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history.get, aggregation))
            nicheElite <- Elitism[M, I] { population => keepHighestRanked(population, ranks, muByNiche).pure[M] }
          } yield nicheElite
      ) apply cloneRemoved
      aged <- incrementGeneration[M, I](age) apply elite
    } yield aged
  }

  def expression[G, I](
    values: G => Vector[Double],
    builder: (G, Double) => I)(fitness: (util.Random, Vector[Double]) => Double): Expression[(util.Random, G), I] = {
    case (rg, g) => builder(g, fitness(rg, values(g)))
  }

  def step[M[_]: cats.Monad: Random: Generation, I, G](
    breeding: Breeding[M, I, G],
    expression: Expression[(util.Random, G), I],
    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)

  def profile[I](population: Vector[I], niche: Niche[I, Int], historyAge: I => Long): Vector[I] =
    population.groupBy(niche).toVector.unzip._2.map {
      _.maxBy(historyAge)
    }
}