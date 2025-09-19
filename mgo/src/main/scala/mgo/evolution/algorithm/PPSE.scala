package mgo.evolution.algorithm


/*
 * Copyright (C) 09/11/2020 Romain Reuillon
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

import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.ranking._
import mgo.tools._
import mgo.tools.execution._

import monocle._
import monocle.syntax.all._

import mgo.tools
import mgo.tools.clustering.{EMGMM, GMM, HDBScan}

import scala.reflect.ClassTag
import scala.util.Random


object PPSE:

  type SamplingWeightMap = Map[Vector[Int], Double]
  type HitMap = Map[Vector[Int], Int]

  case class PPSEState(
    hitmap: HitMap = Map(),
    gmm: Option[GMM] = None,
    likelihoodRatioMap: SamplingWeightMap = Map())

  case class Result[P](continuous: Vector[Double], pattern: Vector[Int], density: Double, phenotype: P, individual: Individual[P])

  def result[P](
    population: Vector[Individual[P]],
    state: EvolutionState[PPSEState],
    continuous: Vector[C],
    pattern: P => Vector[Int]) =

    def computePDF(likelihoodRatioMap: SamplingWeightMap) =
      val totalDensity = likelihoodRatioMap.values.sum
      likelihoodRatioMap.map((p, density) => (p, density / totalDensity))

    val densityMap = computePDF(state.s.likelihoodRatioMap)

    population.map: i =>
      val pa = pattern(i.phenotype)

      Result(
        scaleContinuousValues(i.genome._1, continuous).toVector,
        pa,
        densityMap.getOrElse(pa, 0.0),
        i.phenotype,
        i)


  def result[P](ppse: PPSE[P], population: Vector[Individual[P]], state: EvolutionState[PPSEState]): Vector[Result[P]] =
    result(population, state, ppse.continuous, ppse.pattern)

  type Genome = (IArray[Double], Double)

  case class Individual[P](
    genome: Genome,
    phenotype: P,
    generation: Long,
    initial: Boolean)

  def buildIndividual[P](g: Genome, f: P, generation: Long, initial: Boolean) = Individual(g, f, generation, initial)

  def initialGenomes(number: Int, continuous: Vector[C], reject: Option[IArray[Double] => Boolean], warmupSampler: Int, rng: scala.util.Random) =
    def sample() = (PPSEOperation.randomUnscaledContinuousValues(continuous.size, rng), Lazy(1.0))

    val sampler = PPSEOperation.toSampler(sample, reject, continuous, rng)
    val samplerState = RejectionSampler.warmup(sampler, warmupSampler)

    (0 to number).map: _ =>
      val (g, d) = sample()
      (g, d.value)
    .toVector

  def breeding[P](
    continuous: Vector[C],
    lambda: Int,
    reject: Option[IArray[Double] => Boolean],
    warmupSampler: Int): Breeding[EvolutionState[PPSEState], Individual[P], Genome] =
    PPSEOperation.breeding(
      continuous,
      identity,
      lambda,
      reject,
      _.s.gmm,
      warmupSampler)

  def elitism[P: CanContainNaN](
    pattern: P => Vector[Int],
    continuous: Vector[C],
    reject: Option[IArray[Double] => Boolean],
    density: Option[IArray[Double] => Double],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    minClusterSize: Int,
    maxRareSample: Int,
    regularisationEpsilon: Double) =
    PPSEOperation.elitism[EvolutionState[PPSEState], Individual[P], P](
      values = _.genome,
      phenotype = _.phenotype,
      continuous = continuous,
      reject = reject,
      pattern = pattern,
      likelihoodRatioMap = Focus[EvolutionState[PPSEState]](_.s.likelihoodRatioMap),
      hitmap = Focus[EvolutionState[PPSEState]](_.s.hitmap),
      gmm = Focus[EvolutionState[PPSEState]](_.s.gmm),
      iterations = iterations,
      tolerance = tolerance,
      dilation = dilation,
      maxRareSample = maxRareSample,
      minClusterSize = minClusterSize,
      regularisationEpsilon = regularisationEpsilon,
      density = density)

  def expression[P](phenotype: IArray[Double] => P, continuous: Vector[C]) = (genome: Genome, generation: Long, initial: Boolean) =>
    val sc = scaleContinuousValues(genome._1, continuous)
    Individual(genome, phenotype(sc), generation, initial)

  given [P: CanContainNaN]:Algorithm[PPSE[P], Individual[P], Genome, EvolutionState[PPSEState]] with
    def initialState(t: PPSE[P], rng: util.Random) = EvolutionState[PPSEState](s = PPSEState())

    override def initialPopulation(t: PPSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[P]](
        PPSE.initialGenomes(t.lambda, t.continuous, t.reject, t.warmupSampler, rng),
        PPSE.expression(t.phenotype, t.continuous), parallel)

    def step(t: PPSE[P]) =
      deterministic.step[EvolutionState[PPSEState], Individual[P], Genome](
        PPSE.breeding(t.continuous, t.lambda, t.reject, t.warmupSampler),
        PPSE.expression(t.phenotype, t.continuous),
        PPSE.elitism(t.pattern, t.continuous, t.reject, t.density, t.iterations, t.tolerance, t.dilation, t.minClusterSize, t.warmupSampler, t.maxRareSample),
        Focus[EvolutionState[PPSEState]](_.generation),
        Focus[EvolutionState[PPSEState]](_.evaluated)
      )


case class PPSE[P](
  lambda: Int,
  phenotype: IArray[Double] => P,
  pattern: P => Vector[Int],
  continuous: Vector[C],
  reject: Option[IArray[Double] => Boolean] = None,
  density: Option[IArray[Double] => Double] = None,
  iterations: Int = 1000,
  tolerance: Double = 0.0001,
  warmupSampler: Int = 10000,
  dilation: Double = 4.0,
  maxRareSample: Int = 10,
  minClusterSize: Int = 10,
  regularisationEpsilon: Double = 10e-6)

object PPSEOperation:
  def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = IArray.fill(genomeLength)(() => rng.nextDouble()).map(_())

  def toSampler(sample: () => (IArray[Double], Lazy[Double]), reject: Option[IArray[Double] => Boolean], continuous: Vector[C], rng: Random) =
    def acceptPoint(x: IArray[Double]) = x.forall(_ <= 1.0) && x.forall(_ >= 0.0)

    def acceptFunction(x: IArray[Double]) =
      def rejectValue =
        reject.map: r =>
          r(scaleContinuousValues(x, continuous))
        .getOrElse(false)

      acceptPoint(x) && !rejectValue

    new RejectionSampler(sample, acceptFunction)

  def gmmToSampler(gmm: GMM, reject: Option[IArray[Double] => Boolean], continuous: Vector[C], rng: Random) =
    import mgo.tools.clustering.GMM
    val distribution = GMM.toDistribution(gmm, rng)

    def sample() =
      val x = distribution.sample()
      (IArray.unsafeFromArray(x), Lazy(distribution.density(x)))

    toSampler(sample, reject, continuous, rng)

  def breeding[S, I, G](
    continuous: Vector[C],
    buildGenome: ((IArray[Double], Double)) => G,
    lambda: Int,
    reject: Option[IArray[Double] => Boolean],
    gmm: S => Option[GMM],
    warmupSampler: Int): Breeding[S, I, G] =
    (s, population, rng) =>
      gmm(s) match
        case None =>
          def sample() = (randomUnscaledContinuousValues(continuous.size, rng), Lazy(1.0))
          val sampler = toSampler(sample, reject, continuous, rng)
          val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
          (0 to lambda).map: _ =>
            val (g, d) = sample()
            buildGenome(g, d.value)
          .toVector
        case Some(gmmValue) =>
          val sampler = gmmToSampler(gmmValue, reject, continuous, rng)
          val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
          val (_, sampled) = RejectionSampler.sampleArray(sampler, lambda, samplerState)
          val breed = sampled.toVector.map(s => buildGenome(s._1, s._2))
          breed


  def elitism[S, I, P: CanContainNaN](
    values: I => (IArray[Double], Double),
    phenotype: I => P,
    pattern: P => Vector[Int],
    continuous: Vector[C],
    reject: Option[IArray[Double] => Boolean],
    likelihoodRatioMap: monocle.Lens[S, PPSE.SamplingWeightMap],
    hitmap: monocle.Lens[S, HitMap],
    gmm: monocle.Lens[S, Option[GMM]],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    maxRareSample: Int,
    minClusterSize: Int,
    regularisationEpsilon: Double,
    density: Option[IArray[Double] => Double]): Elitism[S, I] =  (state, population, candidates, rng) =>

    def computeGMM(
      genomes: Array[Array[Double]],
      patterns: Array[Array[Int]],
      hitMap: HitMap,
      maxRareSample: Int,
      regularisationEpsilon: Double,
      iterations: Int,
      tolerance: Double,
      dilation: Double,
      minClusterSize: Int,
      random: Random) =

      val rareIndividuals =
        (genomes zip patterns).filter: p =>
          val hits = hitMap.getOrElse(p._2.toVector, 0)
          hits <= maxRareSample
        .map(_._1)

      val res =
        if rareIndividuals.isEmpty
        then None
        else
          Some:
            def fittedGMM =
              if rareIndividuals.length < minClusterSize
              then GMM.empty
              else
                val (clusterMeans, clusterCovariances, clusterWeights) = HDBScan.clusterize(rareIndividuals, minClusterSize)

                EMGMM.fit(
                  components = clusterMeans.length,
                  iterations = iterations,
                  tolerance = tolerance,
                  x = rareIndividuals,
                  means = clusterMeans,
                  covariances = clusterCovariances,
                  weights = clusterWeights,
                  regularisationEpsilon = regularisationEpsilon)._1

            def gmmWithOutliers = EMGMM.integrateOutliers(rareIndividuals, fittedGMM, regularisationEpsilon)

            GMM.dilate(gmmWithOutliers, dilation)



      res

    def updateState(
      genomes: Array[Array[Double]],
      patterns: Array[Array[Int]],
      offspringGenomes: Array[(IArray[Double], Double)],
      offspringPatterns: Array[Array[Int]],
      likelihoodRatioMap: PPSE.SamplingWeightMap,
      hitMap: HitMap,
      maxRareSample: Int,
      regularisationEpsilon: Double,
      iterations: Int,
      tolerance: Double,
      dilation: Double,
      minClusterSize: Int,
      inputDensity: Option[IArray[Double] => Double],
      continuous: Vector[C],
      random: Random): (HitMap, PPSE.SamplingWeightMap, Option[GMM]) =
      val newHitMap =
        def updateHits(m: HitMap, p: Vector[Int]) = m.updatedWith(p)(v => Some(v.getOrElse(0) + 1))

        offspringPatterns.foldLeft(hitMap)((m, p) => updateHits(m, p.toVector))

      def newLikelihoodRatioMap =
        def offSpringDensities =
          val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
          groupedGenomes.view.mapValues: v =>
            v.map: (genome, density) =>
              val inputDensityValue =
                inputDensity.map: df =>
                  val scaled = scaleContinuousValues(genome, continuous)
                  df(scaled)
                .getOrElse(1.0)

              inputDensityValue / density
            .sum
          .toSeq

        def updatePatternDensity(map: PPSE.SamplingWeightMap, pattern: Array[Int], density: Double): PPSE.SamplingWeightMap =
          map.updatedWith(pattern.toVector)(v => Some(v.getOrElse(0.0) + density))

        offSpringDensities.foldLeft(likelihoodRatioMap) { case (map, (pattern, density)) => updatePatternDensity(map, pattern, density) }

      def newGMM =
        computeGMM(
          genomes = genomes,
          patterns = patterns,
          hitMap = newHitMap,
          maxRareSample = maxRareSample,
          regularisationEpsilon = regularisationEpsilon,
          iterations = iterations,
          tolerance = tolerance,
          dilation = dilation,
          minClusterSize = minClusterSize,
          random = random
        )

      (newHitMap, newLikelihoodRatioMap, newGMM)

    def offSpringWithNoNan = filterNaN(candidates, phenotype)
    def keepRandom(i: Vector[I]) = Vector(i(rng.nextInt(i.size)))

    val newPopulation = keepNiches(phenotype andThen pattern, keepRandom)(population ++ offSpringWithNoNan)

    def genomes(p: Vector[I]) =
      import tools.unsafeToArray
      p.map(values).map(_._1.unsafeToArray).toArray

    def patterns(p: Vector[I]) = p.map(phenotype andThen pattern andThen (_.toArray)).toArray

    val (elitedHitMap, elitedDensity, elitedGMM) =
      updateState(
        genomes = genomes(newPopulation),
        patterns = patterns(newPopulation),
        offspringGenomes = offSpringWithNoNan.map(values).toArray,
        offspringPatterns = patterns(offSpringWithNoNan),
        likelihoodRatioMap = likelihoodRatioMap.get(state),
        hitMap = hitmap.get(state),
        iterations = iterations,
        tolerance = tolerance,
        dilation = dilation,
        minClusterSize = minClusterSize,
        maxRareSample = maxRareSample,
        regularisationEpsilon = regularisationEpsilon,
        inputDensity = density,
        continuous = continuous,
        random = rng)

    def state2 =
      (gmm.modify(gmm => elitedGMM orElse gmm) andThen
        likelihoodRatioMap.replace(elitedDensity) andThen
        hitmap.replace(elitedHitMap))(state)

    (state2, newPopulation)


