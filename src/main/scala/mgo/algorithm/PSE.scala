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

object pse extends niche.Imports {

  implicit def toComponent[C](i: C)(implicit inj: shapeless.ops.coproduct.Inject[pseOperations.Component, C]) = Coproduct[pseOperations.Component](i)
  implicit def toComponentVector[C](v: Vector[C])(implicit inj: shapeless.ops.coproduct.Inject[pseOperations.Component, C]) = v.map(toComponent(_))

  type HitMapM[M[_]] = HitMap[M, Vector[Int]]
  type HitMapState = Map[Vector[Int], Int]

  def result(population: Vector[Individual], genome: Vector[pseOperations.Component]) =
    population.map { i => (pseOperations.doubleValues(i.genome.values.toVector, genome), i.phenotype.toVector) }

  def state[M[_]: cats.Monad: StartTime: Random: Generation](implicit hitmap: mgo.contexts.HitMap[M, Vector[Int]]) = for {
    map <- hitmap.get
    s <- mgo.algorithm.state[M, Map[Vector[Int], Int]](map)
  } yield s

  object PSE {

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
        deterministicInitialPopulation[M, Genome, Individual](
          pse.initialGenomes[M](t.lambda, t.genome.size), pse.expression(t.phenotype, t.genome))

      def step(t: PSE) =
        deterministicStep[M, Individual, Genome](
          pse.breeding(t.lambda, t.pattern, t.operatorExploration),
          pse.expression(t.phenotype, t.genome),
          pse.elitism(t.pattern))

      def state = pse.state[M]

      // def run[A](m: M[A], s: EvolutionState[HitMap]) = interpreter(s).run(m).right.get
    }

  }

  case class PSE(
    lambda: Int,
    phenotype: Expression[Vector[Double], Vector[Double]],
    pattern: Vector[Double] => Vector[Int],
    genome: Vector[pseOperations.Component],
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

  def initialGenomes[M[_]: cats.Monad: StartTime: Random: Generation](mu: Int, genomeSize: Int) =
    GenomeVectorDouble.randomGenomes[M, Genome](buildGenome)(mu, genomeSize)

  def breeding[M[_]: cats.Monad: StartTime: Random: Generation: HitMapM](
    lambda: Int,
    pattern: Vector[Double] => Vector[Int],
    operatorExploration: Double) =
    pseOperations.breeding[M, Individual, Genome](
      Individual.genome.get,
      vectorValues.get,
      Genome.operator.get,
      vectorPhenotype.get _ andThen pattern,
      buildGenome)(lambda, operatorExploration)

  def elitism[M[_]: cats.Monad: StartTime: Random: Generation](pattern: Vector[Double] => Vector[Int])(implicit hm: HitMap[M, Vector[Int]]) =
    pseOperations.elitism[M, Individual, Vector[Double]](
      (Individual.genome composeLens vectorValues).get,
      vectorPhenotype.get,
      pattern,
      Individual.age,
      Individual.mapped)

  def expression(phenotype: Expression[Vector[Double], Vector[Double]], genome: Vector[pseOperations.Component]): Expression[Genome, Individual] =
    pseOperations.expression[Genome, Individual](vectorValues.get, genome, buildIndividual)(phenotype)

}

object pseOperations {

  import shapeless._
  type Component = C :+: CNil

  def breeding[M[_]: cats.Monad: Random: Generation: pse.HitMapM, I, G](
    genome: I => G,
    genomeValues: G => Vector[Double],
    genomeOperator: G => Option[Int],
    pattern: I => Vector[Int],
    buildGenome: (Vector[Double], Option[Int]) => G)(
    lambda: Int,
    operatorExploration: Double): Breeding[M, I, G] = Breeding { population =>

    for {
      ranks <- reversedRanking(hitCountRanking[M, I, Vector[Int]](pattern)) apply population
      operatorStatistics = operatorProportions(genome andThen genomeOperator, population)
      breeding = applyDynamicOperators[M, I](
        tournament[M, I, Lazy[Int]](ranks, rounds = size => math.round(math.log10(size).toInt)),
        genome andThen genomeValues,
        operatorStatistics,
        operatorExploration) apply population
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

  def elitism[M[_]: cats.Monad: Random: Generation: pse.HitMapM, I, P: CanBeNaN](
    values: I => Vector[Double],
    phenotype: I => P,
    pattern: P => Vector[Int],
    age: monocle.Lens[I, Long],
    mapped: monocle.Lens[I, Boolean]): Elitism[M, I] = Elitism[M, I] { population =>
    for {
      cloneRemoved <- applyCloneStrategy(values, keepYoungest[M, I](age.get)) apply filterNaN(population, values)
      mappedPopulation <- addHits[M, I, Vector[Int]](phenotype andThen pattern, mapped) apply cloneRemoved
      elite <- keepNiches(phenotype andThen pattern, randomO[M, I](1)) apply mappedPopulation
    } yield elite
  } andThen incrementGeneration[M, I](age)

  def doubleValues(values: Vector[Double], genomeComponents: Vector[Component]) =
    (values zip genomeComponents.flatMap(_.select[C])).map { case (v, c) => v.scale(c) }

  def expression[G, I](
    values: G => Vector[Double],
    genome: Vector[Component],
    build: (G, Vector[Double]) => I)(express: Expression[Vector[Double], Vector[Double]]): Expression[G, I] = {
    (g: G) =>
      val vs = doubleValues(values(g), genome)
      build(g, express(vs))
  }
}
