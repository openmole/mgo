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
import shapeless._
import freedsl.tool._
import mgo.niche
import mgo.tools._

import scala.language.higherKinds

object NoisyProfile extends niche.Imports {

  import CDGenome._
  import NoisyIndividual._

  def aggregatedFitness[N](profile: NoisyProfile[N]) =
    NoisyNSGA2Operations.aggregated(vectorFitness.get, profile.aggregation)(_)

  def result[N](profile: NoisyProfile[N], population: Vector[Individual]) = {
    def nicheResult(population: Vector[Individual]) =
      keepFirstFront(population, aggregatedFitness(profile))

    nicheElitism[Id, Individual, N](population, nicheResult, profile.niche).map { i =>
      NoisyIndividual.aggregate(i, profile.aggregation, profile.continuous)
    }
  }

  def genomeProfile(x: Int, nX: Int): Niche[Individual, Int] =
    genomeProfile[Individual]((Individual.genome composeLens continuousValues).get _, x, nX)

  def adaptiveBreeding[M[_]: cats.Monad: Random: Generation](lambda: Int, operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Vector[Double]] => Vector[Double], discrete: Vector[D]): Breeding[M, Individual, Genome] =
    NoisyNSGA2Operations.adaptiveBreeding[M, Individual, Genome](
      vectorFitness.get,
      aggregation,
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      logOfPopulationSize,
      lambda,
      operatorExploration,
      cloneProbability)

  def elitism[M[_]: cats.Monad: Random: Generation, N](niche: Niche[Individual, N], muByNiche: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], components: Vector[C]): Elitism[M, Individual] =
    NoisyProfileOperations.elitism[M, Individual, N](
      vectorFitness,
      aggregation,
      i => values(Individual.genome.get(i), components),
      Individual.age,
      Individual.historyAge,
      historySize,
      niche,
      muByNiche)

  def expression(fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double], continuous: Vector[C]): (util.Random, Genome) => Individual =
    NoisyIndividual.expression(fitness, continuous)

  def initialGenomes[M[_]: cats.Monad: Random](lambda: Int, continuous: Vector[C], discrete: Vector[D]) =
    CDGenome.initialGenomes[M](lambda, continuous, discrete)

  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

  //
  //  def genomeProfile(x: Int, nX: Int): Niche[Individual, Int] =
  //    genomeProfile[Individual]((Individual.genome composeLens vectorValues).get _, x, nX)
  //
  //  def result(algorithm: NoisyProfile, population: Vector[Individual]) =
  //    profile(population, algorithm.niche).map { i =>
  //      noisynsga2Operations.doubleValues(i.genome.values.toVector, algorithm.continuous) -> algorithm.aggregation(i.fitnessHistory.toVector)
  //    }
  //
  //  def breeding[M[_]: cats.Monad: Random: Generation](lambda: Int, niche: Niche[Individual, Int], operatorExploration: Double, cloneProbability: Double, aggregation: Vector[Double] => Double): Breeding[M, Individual, Genome] =
  //    noisyprofileOperations.breeding[M, Individual, Genome](
  //      vectorFitness.get, aggregation, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome)(lambda = lambda, niche = niche, operatorExploration = operatorExploration, cloneProbability = cloneProbability)
  //
  //  def expression(fitness: (util.Random, Vector[Double]) => Double, genome: Vector[C]): Expression[(util.Random, Genome), Individual] =
  //    noisyprofileOperations.expression[Genome, Individual](vectorValues.get, genome, buildIndividual)(fitness)
  //
  //  def elitism[M[_]: cats.Monad: Random: Generation](muByNiche: Int, niche: Niche[Individual, Int], historySize: Int, aggregation: Vector[Double] => Double): Elitism[M, Individual] =
  //    noisyprofileOperations.elitism[M, Individual](
  //      history = vectorFitness,
  //      aggregation = aggregation,
  //      values = (Individual.genome composeLens vectorValues).get,
  //      age = Individual.age,
  //      historyAge = Individual.historyAge)(muByNiche, niche, historySize)
  //
  //  def profile(population: Vector[Individual], niche: Niche[Individual, Int]) =
  //    noisyprofileOperations.profile(population, niche, Individual.historyAge.get)
  //
  //  def state[M[_]: cats.Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())
  //
  //  object NoisyProfile {
  //
  //    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
  //    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)
  //

  implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, N]: Algorithm[NoisyProfile[N], M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[NoisyProfile[N], M, Individual, Genome, EvolutionState[Unit]] {
    def initialPopulation(t: NoisyProfile[N]) =
      noisy.initialPopulation[M, Genome, Individual](
        NoisyProfile.initialGenomes[M](t.lambda, t.continuous, t.discrete),
        NoisyProfile.expression(t.fitness, t.continuous))

    def step(t: NoisyProfile[N]) =
      noisy.step[M, Individual, Genome](
        NoisyProfile.adaptiveBreeding[M](
          t.lambda,
          t.operatorExploration,
          t.cloneProbability,
          t.aggregation,
          t.discrete),
        NoisyProfile.expression(t.fitness, t.continuous),
        NoisyProfile.elitism[M, N](
          t.niche,
          t.muByNiche,
          t.historySize,
          t.aggregation,
          t.continuous))

    def state = NoisyProfile.state[M]
  }

}

case class NoisyProfile[N](
  muByNiche: Int,
  lambda: Int,
  fitness: (util.Random, Vector[Double], Vector[Int]) => Vector[Double],
  aggregation: Vector[Vector[Double]] => Vector[Double],
  niche: Niche[CDGenome.NoisyIndividual.Individual, N],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  historySize: Int = 100,
  cloneProbability: Double = 0.2,
  operatorExploration: Double = 0.1)

object NoisyProfileOperations {

  //  def aggregatedFitness[I](fitness: I => Vector[Double], aggregation: Vector[Double] => Double)(i: I): Vector[Double] =
  //    Vector(aggregation(fitness(i)), 1.0 / fitness(i).size.toDouble)
  //
  //  def breeding[M[_]: cats.Monad: Random: Generation, I, G](
  //    history: I => Vector[Double],
  //    aggregation: Vector[Double] => Double,
  //    genome: I => G,
  //    genomeValues: G => Vector[Double],
  //    genomeOperator: G => Option[Int],
  //    buildGenome: (Vector[Double], Option[Int]) => G)(
  //    lambda: Int,
  //    niche: Niche[I, Int],
  //    operatorExploration: Double,
  //    cloneProbability: Double): Breeding[M, I, G] = Breeding { population =>
  //    for {
  //      ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history, aggregation)) apply population
  //      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
  //      breeding = applyContinuousDynamicOperators[M, I](
  //        tournament[M, I, (Lazy[Int], Lazy[Double])](ranks, rounds = size => math.round(math.log10(size).toInt)),
  //        genome andThen genomeValues,
  //        operatorStatistics,
  //        operatorExploration) apply population
  //      offspring <- breeding repeat ((lambda + 1) / 2)
  //      offspringGenomes = offspring.flatMap {
  //        case ((o1, o2), op) =>
  //          def gv1 = o1.map(clamp(_))
  //          def gv2 = o2.map(clamp(_))
  //          Vector(buildGenome(gv1, Some(op)), buildGenome(gv2, Some(op)))
  //      }
  //      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
  //      withClones <- clonesReplace[M, I, G](cloneProbability, population, genome) apply sizedOffspringGenomes
  //    } yield withClones
  //  }

  def elitism[M[_]: cats.Monad: Random: Generation, I, N](
    history: monocle.Lens[I, Vector[Vector[Double]]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long],
    historySize: Int,
    niche: Niche[I, N],
    muByNiche: Int): Elitism[M, I] = {
    def nsga2Elitism(population: Vector[I]) =
      NoisyNSGA2Operations.elitism[M, I](
        history,
        aggregation,
        values,
        age,
        historyAge,
        historySize,
        muByNiche).apply(population)

    Elitism[M, I] { nicheElitism(_, nsga2Elitism, niche) }

    //    Elitism[M, I] { population =>
    //    for {
    //      cloneRemoved <- applyCloneStrategy(values, mergeHistories[M, I, Double](historyAge, history)(historySize)) apply filterNaN(population, aggregatedFitness(history.get, aggregation))
    //      elite <- keepNiches[M, I, Int](
    //        niche = niche,
    //        objective =
    //          for {
    //            ranks <- paretoRankingMinAndCrowdingDiversity[M, I](aggregatedFitness(history.get, aggregation))
    //            nicheElite <- Elitism[M, I] { population => keepHighestRanked(population, ranks, muByNiche).pure[M] }
    //          } yield nicheElite) apply cloneRemoved
    //      aged <- incrementGeneration[M, I](age) apply elite
    //    } yield aged
  }

  //  def doubleValues(values: Vector[Double], continuous: Vector[C]) =
  //    (values zip continuous).map { case (v, c) => v.scale(c) }
  //
  //  def expression[G, I](
  //    values: G => Vector[Double],
  //    continuous: Vector[C],
  //    builder: (G, Double) => I)(fitness: (util.Random, Vector[Double]) => Double): Expression[(util.Random, G), I] = {
  //    case (rg, g) =>
  //      val vs = doubleValues(values(g), continuous)
  //      builder(g, fitness(rg, vs))
  //  }
  //
  //  def step[M[_]: cats.Monad: Random: Generation, I, G](
  //    breeding: Breeding[M, I, G],
  //    expression: Expression[(util.Random, G), I],
  //    elitism: Elitism[M, I]): Kleisli[M, Vector[I], Vector[I]] = noisyStep(breeding, expression, elitism)
  //
  //  def profile[I](population: Vector[I], niche: Niche[I, Int], historyAge: I => Long): Vector[I] =
  //    population.groupBy(niche).toVector.unzip._2.map {
  //      _.maxBy(historyAge)
  //    }
}