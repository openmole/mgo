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

object nichedNSGA2 {

  //  @Lenses case class Genome[N](niche: N, values: Array[Double], operator: Option[Int])
  //  @Lenses case class Individual[N](genome: Genome[N], fitness: Array[Double], age: Long)
  //
  //  def buildIndividual[N](g: Genome[N], f: Vector[Double]) = Individual(g, f.toArray, 0)
  //  def buildGenome[N](niche: N, values: Vector[Double], operator: Option[Int]) = Genome[N](niche, values.toArray, operator)
  //
  //  def vectorFitness = Individual.fitness composeLens arrayToVectorLens
  //  def vectorValues = Genome.values composeLens arrayToVectorLens
  //
  //  def initialGenomes[M[_]: cats.Monad: Random, N](lambda: Int, genomeSize: Int, generateNiche: M[N]): M[Vector[Genome[N]]] =
  //    for {
  //      vectors <- GenomeVectorDouble.randomGenomes[M](lambda, genomeSize)
  //      niches <- generateNiche.repeat(lambda)
  //      genomes = (vectors zip niches).map { case (v, n) => Genome[N](n, v.toArray, None) }
  //    } yield genomes

  import algorithm.nsga2._

  //  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad](lambda: Int, operatorExploration: Double): Breeding[M, Individual, Genome] =
  //    nsga2Operations.adaptiveBreeding[M, Individual, Genome](
  //      vectorFitness.get, Individual.genome.get, vectorValues.get, Genome.operator.get, buildGenome)(lambda, operatorExploration)
  //
  //  def expression(fitness: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
  //    nsga2Operations.expression[Genome, Individual](vectorValues.get, buildIndividual)(fitness)

  def elitism[M[_]: cats.Monad: Random: Generation, N](niche: Individual => N, mu: Int): Elitism[M, Individual] =
    nichedNSGA2Operations.elitism[M, Individual, N](
      vectorFitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(niche, mu)

  def result(population: Vector[Individual], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.fitness.toVector) }

  //  def state[M[_]: Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NichedNSGA2 {

    implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, N]: Algorithm[NichedNSGA2[N], M, Individual, Genome, EvolutionState[Unit]] =
      new Algorithm[NichedNSGA2[N], M, Individual, Genome, EvolutionState[Unit]] {
        override def initialPopulation(t: NichedNSGA2[N]) =
          deterministicInitialPopulation[M, Genome, Individual](nsga2.initialGenomes[M](t.lambda, t.genomeSize), expression(t.fitness))
        override def step(t: NichedNSGA2[N]) =
          nsga2Operations.step[M, Individual, Genome](nsga2.adaptiveBreeding[M](t.lambda, t.operatorExploration), nsga2.expression(t.fitness), nichedNSGA2.elitism(t.niche, t.mu))
        override def state = nsga2.state[M]
      }

  }

  case class NichedNSGA2[N](
    niche: Individual => N,
    mu: Int,
    lambda: Int,
    fitness: Vector[Double] => Vector[Double],
    genomeSize: Int,
    operatorExploration: Double = 0.1)

}

object nichedNSGA2Operations {

  def elitism[M[_]: Monad: Random: Generation, I, N](
    fitness: I => Vector[Double],
    values: I => Vector[Double],
    age: monocle.Lens[I, Long])(niche: I => N, mu: Int) = Elitism[M, I] { population =>
    def nicheElitism(population: Vector[I]) = nsga2Operations.elitism[M, I](fitness, values, age)(mu).apply(population)
    byNiche(population, nicheElitism, niche)
  }

}