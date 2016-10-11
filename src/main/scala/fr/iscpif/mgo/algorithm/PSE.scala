///*
// * Copyright (C) 16/12/2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//package fr.iscpif.mgo.algorithm
//
//import fr.iscpif.mgo._
//import fr.iscpif.mgo.breeding._
//import fr.iscpif.mgo.contexts._
//import fr.iscpif.mgo.ranking._
//import fr.iscpif.mgo.elitism._
//import GenomeVectorDouble._
//import fr.iscpif.mgo.tools.CanBeNaN
//
//import monocle.macros.{ Lenses, GenLens }
//import scala.language.higherKinds
//
//import scala.math._
//import scala.util.Random
//
//import scalaz._
//import Scalaz._
//
//import contexts.default._
//
//object pse {
//
//  case class PSE(
//      lambda: Int,
//      phenotype: Expression[Vector[Double], Vector[Double]],
//      pattern: Vector[Double] => Vector[Int],
//      genomeSize: Int,
//      operatorExploration: Double = 0.1) extends Algorithm[EvolutionState[HitMap, ?], Individual, Genome, EvolutionData[HitMap]] {
//
//    def initialState(rng: Random) = EvolutionData[HitMap](random = rng, s = Map.empty)
//    def initialGenomes = pse.initialGenomes(lambda, genomeSize)
//    def breeding = pse.breeding(lambda, pattern, operatorExploration)
//    def expression = pse.expression(phenotype)
//    def elitism = pse.elitism(pattern)
//    def step = deterministicStep[EvolutionState[HitMap, ?], Individual, Genome](breeding, expression, elitism)
//
//    def run[A](x: EvolutionState[HitMap, A], s: EvolutionData[HitMap]): (EvolutionData[HitMap], A) = default.unwrap(x, s)
//  }
//
//  @Lenses case class Genome(values: Array[Double], operator: Maybe[Int])
//  @Lenses case class Individual(
//    genome: Genome,
//    phenotype: Array[Double],
//    age: Long,
//    mapped: Boolean = false,
//    foundedIsland: Boolean = false)
//
//  def buildIndividual(g: Genome, f: Vector[Double]) = Individual(g, f.toArray, 0)
//  def buildGenome(values: Vector[Double], operator: Maybe[Int]) = Genome(values.toArray, operator)
//
//  def vectorPhenotype = Individual.phenotype composeLens arrayToVectorLens
//  def vectorValues = Genome.values composeLens arrayToVectorLens
//
//  type HitMap = Map[Vector[Int], Int]
//
//  implicit def hitMapper: HitMapper[EvolutionState[HitMap, ?], Vector[Int]] =
//    new HitMapper[EvolutionState[HitMap, ?], Vector[Int]] {
//      def map = monocle.Lens.id[HitMap]
//    }
//
//  def initialGenomes(mu: Int, genomeSize: Int): EvolutionState[HitMap, Vector[Genome]] =
//    GenomeVectorDouble.randomGenomes[EvolutionState[HitMap, ?], Genome](buildGenome)(mu, genomeSize)
//
//  def breeding(
//    lambda: Int,
//    pattern: Vector[Double] => Vector[Int],
//    operatorExploration: Double) =
//    pseOperations.breeding[EvolutionState[HitMap, ?], Individual, Genome](
//      Individual.genome.get,
//      vectorValues.get,
//      Genome.operator.get,
//      vectorPhenotype.get _ andThen pattern,
//      buildGenome
//    )(lambda, operatorExploration)
//
//  def elitism(pattern: Vector[Double] => Vector[Int]) =
//    pseOperations.elitism[EvolutionState[HitMap, ?], Individual, Vector[Double]](
//      (Individual.genome composeLens vectorValues).get,
//      vectorPhenotype.get,
//      pattern,
//      Individual.age,
//      Individual.mapped
//    )
//
//  def expression(phenotype: Expression[Vector[Double], Vector[Double]]): Expression[Genome, Individual] =
//    pseOperations.expression[Genome, Individual](vectorValues.get, buildIndividual)(phenotype)
//
//  case class OpenMOLE(
//    pattern: Vector[Double] => Vector[Int],
//    genomeSize: Int,
//    operatorExploration: Double)
//
//  object OpenMOLE {
//
//    import fr.iscpif.mgo.contexts.default._
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
//      def unwrap[A](x: M[A], s: S): (S, A) = default.unwrap(x, s)
//    }
//  }
//}
//
//object pseOperations {
//
//  def breeding[M[_]: Monad: RandomGen: Generational, I, G](
//    genome: I => G,
//    genomeValues: G => Vector[Double],
//    genomeOperator: G => Maybe[Int],
//    pattern: I => Vector[Int],
//    buildGenome: (Vector[Double], Maybe[Int]) => G)(
//      lambda: Int,
//      operatorExploration: Double)(implicit MH: HitMapper[M, Vector[Int]]): Breeding[M, I, G] =
//    for {
//      operatorStatistics <- operatorProportions[M, I](genome andThen genomeOperator)
//      gs <- tournament(reversedRanking[M, I](hitCountRanking[M, I, Vector[Int]](pattern)), lambda + 1) andThen
//        pairConsecutive andThen
//        mapPureB { case (g1, g2) => ((genome andThen genomeValues)(g1), (genome andThen genomeValues)(g2)) } andThen
//        applyDynamicOperator(operatorStatistics, operatorExploration) andThen
//        flatMapPureB { case ((g1, g2), op) => Vector((g1, op), (g2, op)) } andThen
//        randomTakeLambda(lambda) andThen
//        clamp(GenLens[(Vector[Double], Int)](_._1)) andThen
//        mapPureB { case (g, op) => buildGenome(g, Maybe.just(op)) }
//    } yield gs
//
//  def elitism[M[_]: Monad: RandomGen: Generational, I, P: CanBeNaN](
//    values: I => Vector[Double],
//    phenotype: I => P,
//    pattern: P => Vector[Int],
//    age: monocle.Lens[I, Long],
//    mapped: monocle.Lens[I, Boolean])(implicit MH: HitMapper[M, Vector[Int]]): Elitism[M, I] =
//    addHits[M, I, Vector[Int]](phenotype andThen pattern, mapped) andThen
//      applyCloneStrategy(values, keepYoungest[M, I](age.get)) andThen
//      filterNaN(phenotype) andThen
//      keepNiches(
//        niche = phenotype andThen pattern,
//        objective = randomO[M, I](1)
//      ) andThen incrementGeneration(age)
//
//  def expression[G, I](
//    values: G => Vector[Double],
//    build: (G, Vector[Double]) => I)(express: Expression[Vector[Double], Vector[Double]]): Expression[G, I] =
//    (g: G) => build(g, express(values(g)))
//}