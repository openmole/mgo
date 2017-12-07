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
import monocle._
import monocle.macros._

object nichednsga2 extends niche.Imports {

  import algorithm.nsga2._

  //  @Lenses case class Genome[N](niche: N, values: Array[Double], operator: Option[Int])
  @Lenses case class Individual[P](genome: nsga2.Genome, phenotype: P, fitness: Array[Double], age: Long)
  //
  def buildIndividual[P](g: Genome, p: P, f: Vector[Double]) = Individual(g, p, f.toArray, 0)
  //  def buildGenome[N](niche: N, values: Vector[Double], operator: Option[Int]) = Genome[N](niche, values.toArray, operator)
  //
  //
  //
  def vectorFitness[P] = Individual.fitness[P] composeLens arrayToVectorLens
  //  def vectorValues = Genome.values composeLens arrayToVectorLens
  //
  //  def initialGenomes[M[_]: cats.Monad: Random, N](lambda: Int, genomeSize: Int, generateNiche: M[N]): M[Vector[Genome[N]]] =
  //    for {
  //      vectors <- GenomeVectorDouble.randomGenomes[M](lambda, genomeSize)
  //      niches <- generateNiche.repeat(lambda)
  //      genomes = (vectors zip niches).map { case (v, n) => Genome[N](n, v.toArray, None) }
  //    } yield genomes

  def adaptiveBreeding[M[_]: Generation: Random: cats.Monad, P](lambda: Int, operatorExploration: Double): Breeding[M, Individual[P], Genome] =
    nsga2Operations.adaptiveBreeding[M, Individual[P], Genome](
      vectorFitness.get, Individual.genome[P].get, vectorValues.get, Genome.operator.get, buildGenome)(lambda, operatorExploration)

  def expression[P](phenotype: Vector[Double] => P, fitness: P => Vector[Double]): Expression[Genome, Individual[P]] =
    nichedNSGA2Operations.expression[Genome, Individual[P], P](vectorValues.get, buildIndividual)(phenotype, fitness)

  def elitism[M[_]: cats.Monad: Random: Generation, N, P](niche: Individual[P] => N, mu: Int): Elitism[M, Individual[P]] =
    nichedNSGA2Operations.elitism[M, Individual[P], N](
      vectorFitness.get,
      (Individual.genome composeLens vectorValues).get,
      Individual.age)(niche, mu)

  def result[P](population: Vector[Individual[P]], scaling: Vector[Double] => Vector[Double]) =
    population.map { i => (scaling(i.genome.values.toVector), i.phenotype, i.fitness.toVector) }

  //  def state[M[_]: Monad: StartTime: Random: Generation] = mgo.algorithm.state[M, Unit](())

  object NichedNSGA2 {

    def run[T](rng: util.Random)(f: contexts.run.Implicits => T): T = contexts.run(rng)(f)
    def run[T](state: EvolutionState[Unit])(f: contexts.run.Implicits => T): T = contexts.run(state)(f)

    implicit def isAlgorithm[M[_]: Generation: Random: cats.Monad: StartTime, N, P]: Algorithm[NichedNSGA2[N, P], M, Individual[P], Genome, EvolutionState[Unit]] =
      new Algorithm[NichedNSGA2[N, P], M, Individual[P], Genome, EvolutionState[Unit]] {

        override def initialPopulation(t: NichedNSGA2[N, P]) =
          deterministicInitialPopulation[M, Genome, Individual[P]](
            nsga2.initialGenomes[M](t.lambda, t.genomeSize),
            expression(t.phenotype, t.fitness))

        override def step(t: NichedNSGA2[N, P]) =
          nsga2Operations.step[M, Individual[P], Genome](
            adaptiveBreeding[M, P](t.lambda, t.operatorExploration),
            expression(t.phenotype, t.fitness),
            nichednsga2.elitism((Individual.phenotype[P].asGetter composeGetter Getter(t.niche)).get, t.mu))

        override def state = nsga2.state[M]
      }

  }

  case class NichedNSGA2[N, P](
    niche: P => N,
    mu: Int,
    lambda: Int,
    phenotype: Vector[Double] => P,
    fitness: P => Vector[Double],
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

  def expression[G, I, P](
    values: G => Vector[Double],
    build: (G, P, Vector[Double]) => I)(phenotype: Vector[Double] => P, fitness: P => Vector[Double]): Expression[G, I] =
    (g: G) => {
      val phenotypeValue = phenotype(values(g))
      build(g, phenotypeValue, fitness(phenotypeValue))
    }

}