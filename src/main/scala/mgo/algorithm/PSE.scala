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
import mgo.tools.math._
import mgo.niche._
import GenomeVectorDouble._
import mgo.tools.{ CanBeNaN, Lazy }
import monocle.macros.{ GenLens, Lenses }

import scala.language.higherKinds
import cats._
import cats.implicits._
import freedsl.dsl
import freedsl.dsl.dsl
import freedsl.random._
import freedsl.io._
import freedsl.tool._
import freek._

object pse extends niche.Imports {

  object VectorHitMap {
    def interpreter(m: HitMap) = new Interpreter[Id] {
      var map = m
      def interpret[_] = {
        case get() => Right(map)
        case set(m) => Right(map = m)
      }
    }
  }

  @dsl trait VectorHitMap[M[_]] extends mgo.contexts.HitMap[M, Vector[Int]] {
    override def get: M[Map[Vector[Int], Int]]
    override def set(map: Map[Vector[Int], Int]): M[Unit]
  }

  val context = dsl.merge(Random, StartTime, Generation, IO, VectorHitMap)
  import context._
  import context.implicits._

  type HitMap = Map[Vector[Int], Int]

  def interpreter(s: EvolutionState[HitMap]) =
    Random.interpreter(s.random) :&:
      StartTime.interpreter(s.startTime) :&:
      Generation.interpreter(s.generation) :&:
      IO.interpreter :&:
      VectorHitMap.interpreter(s.s)

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.phenotype.toVector) }

  object PSE {

    implicit def isAlgorithm = new Algorithm[PSE, M, Individual, Genome, EvolutionState[HitMap]] {

      def initialState(t: PSE, rng: util.Random) = EvolutionState[HitMap](random = rng, s = Map.empty)
      override def initialPopulation(t: PSE) =
        deterministicInitialPopulation[M, Genome, Individual](
          pse.initialGenomes(t.lambda, t.genomeSize), pse.expression(t.phenotype))

      def step(t: PSE) =
        deterministicStep[M, Individual, Genome](
          pse.breeding(t.lambda, t.pattern, t.operatorExploration),
          pse.expression(t.phenotype),
          pse.elitism(t.pattern))

      def state =
        for {
          map <- implicitly[mgo.contexts.HitMap[M, Vector[Int]]].get
          s <- mgo.algorithm.state[M, HitMap](map)
        } yield s

      def run[A](m: M[A], s: EvolutionState[HitMap]) = context.result(m, interpreter(s)).right.get
    }

  }

  case class PSE(
    lambda: Int,
    phenotype: Expression[Vector[Double], Vector[Double]],
    pattern: Vector[Double] => Vector[Int],
    genomeSize: Int,
    operatorExploration: Double = 0.1)

  @Lenses case class Genome(values: Array[Double], operator: Option[Int])
  @Lenses case class Individual(
    genome: Genome,
    phenotype: Array[Double],
    age: Long,
    mapped: Boolean = false,
    foundedIsland: Boolean = false)

  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)
  def buildGenome(values: Vector[Double], operator: Option[Int]) = Genome(values.toArray, operator)

  def vectorPhenotype = Individual.phenotype composeLens arrayToVectorLens
  def vectorValues = Genome.values composeLens arrayToVectorLens

  //
  //  implicit def hitMapper: HitMapper[EvolutionState[HitMap, ?], Vector[Int]] =
  //    new HitMapper[EvolutionState[HitMap, ?], Vector[Int]] {
  //      def map = monocle.Lens.id[HitMap]
  //    }

  def initialGenomes(mu: Int, genomeSize: Int) =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(mu, genomeSize)

  def breeding(
    lambda: Int,
    pattern: Vector[Double] => Vector[Int],
    operatorExploration: Double) =
    pseOperations.breeding[M, Individual, Genome](
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      vectorPhenotype.get _ andThen pattern,
      buildGenome
    )(lambda, operatorExploration)

  def elitism(pattern: Vector[Double] => Vector[Int]) =
    pseOperations.elitism[M, Individual, Vector[Double]](
      (Individual.genome composeLens vectorValues).get,
      vectorPhenotype.get,
      pattern,
      Individual.age,
      Individual.mapped
    )

  def expression(phenotype: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
    pseOperations.expression[Genome, Individual](vectorValues.get, buildIndividual)(phenotype)

  //  case class OpenMOLE(
  //    pattern: Vector[Double] => Vector[Int],
  //    genomeSize: Int,
  //    operatorExploration: Double)
  //
  //  object OpenMOLE {
  //
  //    implicit def integration: openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] = new openmole.Integration[OpenMOLE, Vector[Double], Vector[Double]] {
  //      type M[A] = EvolutionState[HitMap, A]
  //      type G = Genome
  //      type I = Individual
  //      type S = EvolutionData[HitMap]
  //
  //      def iManifest = implicitly
  //      def gManifest = implicitly
  //      def sManifest = implicitly
  //      def mMonad = implicitly
  //      def mGenerational = implicitly
  //      def mStartTime = implicitly
  //
  //      def operations(om: OpenMOLE) = new Ops {
  //        def randomLens = GenLens[S](_.random)
  //        def startTimeLens = GenLens[S](_.startTime)
  //        def generation(s: S) = s.generation
  //        def values(genome: G) = vectorValues.get(genome)
  //        def genome(i: I) = Individual.genome.get(i)
  //        def phenotype(individual: I): Vector[Double] = vectorPhenotype.get(individual)
  //        def buildIndividual(genome: G, phenotype: Vector[Double]) = pse.buildIndividual(genome, phenotype)
  //        def initialState(rng: Random) = EvolutionData[HitMap](random = rng, s = Map())
  //        def initialGenomes(n: Int): M[Vector[G]] = pse.initialGenomes(n, om.genomeSize)
  //        def breeding(n: Int): Breeding[M, I, G] = pse.breeding(n, om.pattern, om.operatorExploration)
  //        def elitism: Elitism[M, I] = pse.elitism(om.pattern)
  //        def migrateToIsland(population: Vector[I]) = population.map(Individual.foundedIsland.set(true))
  //        def migrateFromIsland(population: Vector[I]) =
  //          population.filter(i => !Individual.foundedIsland.get(i)).map(Individual.mapped.set(false))
  //      }
  //
  //      def unwrap[A](x: M[A], s: S): (S, A) = mgo.unwrap(x, s)
  //    }
  //  }
}

object pseOperations {

  def breeding[M[_]: Monad: Random: Generation, I, G](
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
      lambda: Int,
      operatorExploration: Double)(implicit MH: HitMap[M, Vector[Int]]): Breeding[M, I, G] = Breeding { population =>
    for {
      ranks <- reversedRanking(hitCountRanking[M, I, Vector[Int]](pattern)) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperator[M, I](
        tournament[M, I, Lazy[Int]](population, ranks, rounds = size => math.round(math.log10(size).toInt)),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration
      )
      offspring <- breeding repeat ((lambda + 1) / 2)
      offspringGenomes = offspring.flatMap {
        case ((o1, o2), op) =>
          def gv1 = o1.map(clamp(_))
          def gv2 = o2.map(clamp(_))
          Vector(buildGenome(gv1, Some(op)), buildGenome(gv2, Some(op)))
      }
      sizedOffspringGenomes <- randomTake[M, G](offspringGenomes, lambda)
    } yield sizedOffspringGenomes
  }

  def elitism[M[_]: Monad: Random: Generation, I, P: CanBeNaN](
    values: I => Vector[Double],
    phenotype: I => P,
    pattern: P => Vector[Int],
    age: monocle.Lens[I, Long],
    mapped: monocle.Lens[I, Boolean])(implicit MH: HitMap[M, Vector[Int]]): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, values)
      mappedPopulation <- addHits[M, I, Vector[Int]](phenotype andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(phenotype andThen pattern, randomO[M, I](1)) apply mappedPopulation
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def expression[G, I](
    values: G => Vector[Double],
    build: (G, Vector[Double]) => I)(express: Expression[Vector[Double], Vector[Double]]): Expression[G, I] =
    (g: G) => build(g, express(values(g)))
}