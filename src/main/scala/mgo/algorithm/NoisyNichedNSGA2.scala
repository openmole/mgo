package mgo.algorithm

import scala.language.higherKinds
import mgo._
import mgo.ranking._
import mgo.breeding._
import mgo.elitism._
import mgo.contexts._
import tools._
import cats._
import cats.data._
import cats.implicits._
import GenomeVectorDouble._
import freedsl.dsl
import freedsl.tool._
import monocle.macros._

object noisyNichedNSGA2 {
  
  import algorithm.noisynsga2._

  def elitism[M[_]: cats.Monad: Random: Generation, N](mu: Int, historySize: Int, aggregation: Vector[Vector[Double]] => Vector[Double], niche: Individual => N): Elitism[M, Individual] =
    noisyNichedNSGA2Operations.elitism[M, Individual, N](
      vectorFitness,
      aggregation,
      (Individual.genome composeLens vectorValues).get,
      Individual.age,
      Individual.historyAge)(niche, mu, historySize)

  def result(population: Vector[Individual], aggregation: Vector[Vector[Double]] => Vector[Double], scaling: Vector[Double] => Vector[Double]) =
    noisynsga2.result(population, aggregation, scaling)

  object NoisyNSGA2 {

    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

    implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, N]: Algorithm[NoisyNichedNSGA2[N], M, Individual, Genome, EvolutionState[Unit]] = new Algorithm[NoisyNichedNSGA2[N], M, Individual, Genome, EvolutionState[Unit]] {
      def initialPopulation(t: NoisyNichedNSGA2[N]) =
        stochasticInitialPopulation[M, Genome, Individual](
          noisynsga2.initialGenomes[M](t.lambda, t.genomeSize),
          noisynsga2.expression(t.fitness))

      def step(t: NoisyNichedNSGA2[N]): Kleisli[M, Vector[Individual], Vector[Individual]] =
        noisynsga2Operations.step[M, Individual, Genome](
          noisynsga2.adaptiveBreeding[M](t.lambda, t.operatorExploration, t.cloneProbability, t.aggregation),
          noisynsga2.expression(t.fitness),
          elitism[M, N](t.mu, t.historySize, t.aggregation, t.niche))

      def state = noisynsga2.state[M]
    }
  }

  case class NoisyNichedNSGA2[N](
    niche: Individual => N,
    mu: Int,
    lambda: Int,
    fitness: (util.Random, Vector[Double]) => Vector[Double],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    genomeSize: Int,
    historySize: Int = 100,
    cloneProbability: Double = 0.2,
    operatorExploration: Double = 0.1)

}

object noisyNichedNSGA2Operations {

  def elitism[M[_]: cats.Monad: Random: Generation, I, N](
    history: monocle.Lens[I, Vector[Vector[Double]]],
    aggregation: Vector[Vector[Double]] => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long],
    historyAge: monocle.Lens[I, Long])(niche: I => N, mu: Int, historySize: Int): Elitism[M, I] = Elitism[M, I] { population =>

    def nicheElitism(population: Vector[I]) =
      noisynsga2Operations.elitism[M, I](
        history,
        aggregation,
        values,
        age,
        historyAge)(mu, historySize).apply(population)

    byNiche(population, nicheElitism, niche)
  }

}
