//package mgo.evolution.algorithm
//
//import breeze.linalg.DenseVector
//import mgo.tools.clustering.{ EMGMM, GMM, WDFEMGMM }
//
//import scala.reflect.ClassTag
//import scala.util.Random
//
///*
// * Copyright (C) 09/11/2020 Romain Reuillon
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
//
//import cats.implicits._
//import mgo.evolution.algorithm._
//import mgo.evolution._
//import mgo.evolution.algorithm.GenomeVectorDouble._
//import mgo.evolution.breeding._
//import mgo.evolution.elitism._
//import mgo.evolution.ranking._
//import mgo.tools._
//import mgo.tools.execution._
//
//import monocle._
//import monocle.syntax.all._
//
//object EMPPSE {
//
//  type DensityMap = Map[Vector[Int], Double]
//  case class EMPPSEState(
//    hitmap: HitMap = Map(),
//    gmm: Option[(GMM, RejectionSampler.State)] = None,
//    probabilityMap: DensityMap = Map())
//
//  case class Result[P](continuous: Vector[Double], pattern: Vector[Int], density: Double, phenotype: Vector[Double], individual: Individual[P])
//
//  def result[P](population: Vector[Individual[P]], state: EvolutionState[EMPPSEState], continuous: Vector[C], phenotype: P => Vector[Double], pattern: Vector[Double] ⇒ Vector[Int]) = {
//    val densityMap = state.focus(_.s) andThen Focus[EMPPSEState](_.probabilityMap) get
//    val total = densityMap.map(_._2).sum
//
//    population.map { i =>
//      val ph = phenotype(i.phenotype)
//      val pa = pattern(ph)
//
//      Result(
//        scaleContinuousValues(i.genome._1.toVector, continuous),
//        pa,
//        densityMap.getOrElse(pa, 0.0) / total,
//        ph,
//        i)
//    }
//  }
//
//  def result(pse: EMPPSE, population: Vector[Individual[Vector[Double]]], state: EvolutionState[EMPPSEState]): Vector[Result[Vector[Double]]] =
//    result(population, state, pse.continuous, identity, pse.pattern)
//
//  type Genome = (Array[Double], Double)
//
//  case class Individual[P](
//    genome: Genome,
//    phenotype: P)
//
//  def buildIndividual[P](g: Genome, f: P) = Individual(g, f)
//  //def vectorPhenotype[P] = Focus[Individual[P]](_.phenotype) andThen arrayToVectorIso[Double]
//
//  def initialGenomes(number: Int, continuous: Vector[C] /*, reject: Option[Genome => Boolean]*/ , rng: scala.util.Random) = {
//    def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())
//
//    //val rejectValue = reject.getOrElse((_: Genome) => false)
//
//    def generate(acc: List[Genome], n: Int): Vector[Genome] =
//      if (n >= number) acc.toVector
//      else {
//        val g = randomUnscaledContinuousValues(continuous.length, rng)
//        //        if (rejectValue(g)) generate(acc, n)
//        //        else generate(g :: acc, n + 1)
//        generate((g, 1.0) :: acc, n + 1)
//      }
//
//    generate(List(), 0)
//  }
//
//  def breeding[P](
//    continuous: Vector[C],
//    lambda: Int,
//    reject: Option[Vector[Double] => Boolean]): Breeding[EvolutionState[EMPPSEState], Individual[P], Genome] =
//    EMPPSEOperation.breeding(
//      continuous,
//      identity[Genome] _,
//      lambda,
//      reject,
//      Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm) get)
//
//  def elitism[P: CanBeNaN](
//    pattern: P => Vector[Int],
//    continuous: Vector[C],
//    reject: Option[Vector[Double] => Boolean],
//    iterations: Int,
//    tolerance: Double,
//    dilation: Double,
//    warmupSampler: Int,
//    fitOnRarest: Int) =
//    EMPPSEOperation.elitism[EvolutionState[EMPPSEState], Individual[P], P](
//      values = _.genome,
//      phenotype = _.phenotype,
//      continuous = continuous,
//      reject = reject,
//      pattern = pattern,
//      densityMap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.probabilityMap),
//      hitmap = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.hitmap),
//      gmm = Focus[EvolutionState[EMPPSEState]](_.s) andThen Focus[EMPPSEState](_.gmm),
//      iterations = iterations,
//      tolerance = tolerance,
//      dilation = dilation,
//      warmupSampler = warmupSampler,
//      fitOnRarest = fitOnRarest)
//
//  def expression[P](phenotype: Vector[Double] => P, continuous: Vector[C]): Genome => Individual[P] = (g: Genome) => {
//    val sc = scaleContinuousValues(g._1.toVector, continuous)
//    Individual(g, phenotype(sc))
//  }
//
//  implicit def isAlgorithm: Algorithm[EMPPSE, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]] = new Algorithm[EMPPSE, Individual[Vector[Double]], Genome, EvolutionState[EMPPSEState]] {
//    def initialState(t: EMPPSE, rng: util.Random) = EvolutionState[EMPPSEState](s = EMPPSEState())
//
//    override def initialPopulation(t: EMPPSE, rng: scala.util.Random) =
//      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
//        EMPPSE.initialGenomes(t.lambda, t.continuous, rng),
//        EMPPSE.expression(t.phenotype, t.continuous))
//
//    def step(t: EMPPSE) =
//      (s, pop, rng) =>
//        deterministic.step[EvolutionState[EMPPSEState], Individual[Vector[Double]], Genome](
//          EMPPSE.breeding(t.continuous, t.lambda, t.reject),
//          EMPPSE.expression[Vector[Double]](t.phenotype, t.continuous),
//          EMPPSE.elitism(t.pattern, t.continuous, t.reject, t.iterations, t.tolerance, t.dilation, t.warmupSampler, t.fitOnRarest),
//          Focus[EvolutionState[EMPPSEState]](_.generation),
//          Focus[EvolutionState[EMPPSEState]](_.evaluated))(s, pop, rng)
//
//  }
//
//  def acceptPoint(x: Vector[Double]) =
//    x.forall(_ <= 1.0) && x.forall(_ >= 0.0)
//
//  def toSampler(gmm: GMM, reject: Option[Vector[Double] => Boolean], continuous: Vector[C], rng: Random) = {
//    val distribution = WDFEMGMM.toDistribution(gmm, rng)
//
//    def sample() = {
//      val x = distribution.sample()
//      (x.toVector, Lazy(distribution.density(x)))
//    }
//
//    def acceptFunction(x: Vector[Double]) =
//      EMPPSE.acceptPoint(x) && !reject.map { r => r(scaleContinuousValues(x, continuous)) }.getOrElse(false)
//
//    new RejectionSampler(sample _, acceptFunction)
//  }
//
//}
//
//case class EMPPSE(
//  lambda: Int,
//  phenotype: Vector[Double] => Vector[Double],
//  pattern: Vector[Double] => Vector[Int],
//  continuous: Vector[C],
//  reject: Option[Vector[Double] => Boolean] = None,
//  iterations: Int = 1000,
//  tolerance: Double = 0.0001,
//  warmupSampler: Int = 10000,
//  dilation: Double = 1.0,
//  fitOnRarest: Int = 100)
//
//object EMPPSEOperation {
//
//  import EMPPSE.DensityMap
//
//  def breeding[S, I, G](
//    continuous: Vector[C],
//    buildGenome: ((Array[Double], Double)) => G,
//    lambda: Int,
//    reject: Option[Vector[Double] => Boolean],
//    gmm: S => Option[(GMM, RejectionSampler.State)]): Breeding[S, I, G] =
//    (s, population, rng) => {
//
//      def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = Array.fill(genomeLength)(() => rng.nextDouble()).map(_())
//
//      gmm(s) match {
//        case None => (0 to lambda).map(_ => buildGenome(randomUnscaledContinuousValues(continuous.size, rng), 1.0)).toVector
//        case Some((gmmValue, rejectionSamplerState)) =>
//          val sampler = EMPPSE.toSampler(gmmValue, reject, continuous, rng)
//          val (_, sampled) = sampler.sampleVector(lambda, rejectionSamplerState)
//          val breed = sampled.map(s => buildGenome(s._1.toArray, s._2))
//          breed
//      }
//    }
//
//  def elitism[S, I, P: CanBeNaN](
//    values: I => (Array[Double], Double),
//    phenotype: I => P,
//    pattern: P => Vector[Int],
//    continuous: Vector[C],
//    reject: Option[Vector[Double] => Boolean],
//    densityMap: monocle.Lens[S, DensityMap],
//    hitmap: monocle.Lens[S, HitMap],
//    gmm: monocle.Lens[S, Option[(GMM, RejectionSampler.State)]],
//    iterations: Int,
//    tolerance: Double,
//    dilation: Double,
//    warmupSampler: Int,
//    minClusterSize: Int = 10,
//    fitOnRarest: Int): Elitism[S, I] = { (state, population, candidates, rng) =>
//
//    def updateGMM(
//      genomes: Array[Array[Double]],
//      patterns: Array[Array[Int]],
//      newHitMap: HitMap,
//      iterations: Int,
//      tolerance: Double,
//      dilation: Double,
//      warmupSampling: Int,
//      minClusterSize: Int,
//      random: Random) = {
//
//      //        def sortedIndividuals =
//      //          (genomes zip patterns).sortBy { case (_, p) => newHitMap.getOrElse(p.toVector, 1) }
//      //
//      //        def rareIndividuals = sortedIndividuals.take(fitOnRarest)
//
//      def rareIndividuals = {
//        val pop = (genomes zip patterns)
//        val rare = pop.filter { case (_, p) => newHitMap.getOrElse(p.toVector, 1) < fitOnRarest }
//        if (rare.size < minClusterSize) pop
//        else rare
//      }
//
//      //        def genomeWeights = {
//      //          val maxHit = newHitMap.values.max
//      //          val minHit = newHitMap.values.min
//      //          rareIndividuals.map { case (_, p) =>
//      //            val hits = newHitMap.getOrElse(p.toVector, 1)
//      //            //math.pow((math.abs(hits - maxHit) + 1) / (maxHit.toDouble - minHit + 1), 1.0 / 4)
//      //           (math.abs(hits - minHit - maxHit) + 1) / (maxHit.toDouble - minHit + 1)
//      //          }
//      //        }
//
//      def genomeWeights =
//        rareIndividuals.map {
//          case (_, p) =>
//            val hits = newHitMap.getOrElse(p.toVector, 1)
//            if (hits < fitOnRarest) fitOnRarest.toDouble - hits else 1.0
//        }
//
//      WDFEMGMM.initializeAndFit(
//        iterations = iterations,
//        tolerance = tolerance,
//        x = rareIndividuals.map(_._1),
//        dataWeights = Some(genomeWeights),
//        minClusterSize = minClusterSize,
//        random = random).toOption flatMap {
//          case (newGMM, _) =>
//            val dilatedGMM = EMGMM.dilate(newGMM, dilation)
//            val samplerState = EMPPSE.toSampler(dilatedGMM, reject, continuous, rng).warmup(warmupSampler)
//            if (RejectionSampler.noSuccess(samplerState)) None
//            else Some((dilatedGMM, samplerState))
//        }
//    }
//
//    def updateState(
//      genomes: Array[Array[Double]],
//      patterns: Array[Array[Int]],
//      offspringGenomes: Array[(Array[Double], Double)],
//      offspringPatterns: Array[Array[Int]],
//      densityMap: DensityMap,
//      hitMap: HitMap,
//      iterations: Int,
//      tolerance: Double,
//      dilation: Double,
//      warmupSampler: Int,
//      minClusterSize: Int,
//      fitOnRarest: Int,
//      random: Random): (HitMap, Option[(GMM, RejectionSampler.State)], DensityMap) = {
//
//      val newHitMap = {
//        def addHits(offspringPatterns: Array[Array[Int]], hitmap: HitMap): HitMap = {
//          def hits(map: HitMap, c: Vector[Int]) = map.updated(c, map.getOrElse(c, 0) + 1)
//          offspringPatterns.foldLeft(hitmap)((m, p) => hits(m, p.toVector))
//        }
//        addHits(offspringPatterns, hitMap)
//      }
//
//      def newDensityMap = {
//        def offSpringDensities = {
//          val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
//          groupedGenomes.view.mapValues(v => v.map(p => 1 / p._2)).toSeq
//        }
//
//        offSpringDensities.foldLeft(densityMap) {
//          case (map, (pattern, densities)) =>
//            map.updatedWith(pattern.toVector)(v => Some(v.getOrElse(0.0) + densities.sum))
//        }
//      }
//
//      if (genomes.size < minClusterSize * 2) (newHitMap, None, newDensityMap)
//      else {
//        def newGMM =
//          updateGMM(
//            genomes = genomes,
//            patterns = patterns,
//            newHitMap = newHitMap,
//            iterations = iterations,
//            tolerance = tolerance,
//            dilation = dilation,
//            warmupSampling = warmupSampler,
//            minClusterSize = minClusterSize,
//            random = random)
//
//        (newHitMap, newGMM, newDensityMap)
//      }
//    }
//
//    def offSpringWithNoNan = filterNaN(candidates, phenotype)
//    def keepFirst(i: Vector[I]) = Vector(i.head)
//
//    val newPopulation = keepNiches(phenotype andThen pattern, keepFirst)(population ++ offSpringWithNoNan)
//
//    def genomes(p: Vector[I]) = p.map(values).map(_._1).toArray
//    def patterns(p: Vector[I]) = p.map(phenotype andThen pattern).map(_.toArray).toArray
//
//    val (elitedHitMap, elitedGMM, elitedDensity) =
//      updateState(
//        genomes = genomes(newPopulation),
//        patterns = patterns(newPopulation),
//        offspringGenomes = offSpringWithNoNan.map(values).toArray,
//        offspringPatterns = patterns(offSpringWithNoNan),
//        densityMap = densityMap.get(state),
//        hitMap = hitmap.get(state),
//        iterations = iterations,
//        tolerance = tolerance,
//        dilation = dilation,
//        warmupSampler = warmupSampler,
//        minClusterSize = minClusterSize,
//        fitOnRarest = fitOnRarest,
//        random = rng)
//
//    def state2 =
//      (gmm.modify(gmm => elitedGMM orElse gmm) andThen
//        densityMap.replace(elitedDensity) andThen
//        hitmap.replace(elitedHitMap))(state)
//
//    (state2, newPopulation)
//  }
//
//}
//
