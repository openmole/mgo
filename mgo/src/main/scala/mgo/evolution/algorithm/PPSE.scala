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

import cats.implicits.*
import mgo.evolution.algorithm.*
import mgo.evolution.*
import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.evolution.ranking.*
import mgo.tools.*
import mgo.tools.execution.*
import monocle.*
import monocle.syntax.all.*
import mgo.tools
import mgo.tools.clustering.{EMGMM, GMM, HDBScan}
import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution

import scala.reflect.ClassTag
import scala.util.Random


object PPSE:

  object Genome:
    def apply(values: IArray[Double], density: Option[Double] = None): Genome = values.unsafeToArray ++ density.orElse(Some(-1.0)).toSeq
    def toTuple(g: Genome): (IArray[Double], Option[Double]) = (values(g), density(g))

    def values(g: Genome): IArray[Double] = IArray.unsafeFromArray(g.take(g.length - 1))
    def density(g: Genome): Option[Double] = g.lastOption.filter(_ >= 0.0)

  type Genome = Array[Double]


  type SamplingWeightMap = PatternMap[Double]
  type HitMap = PatternMap[Int]

  case class PPSEState(
    hitmap: HitMap = PatternMap.empty,
    gmm: Option[GMM] = None,
    likelihoodRatioMap: SamplingWeightMap = PatternMap.empty)

  case class Result[P](continuous: Vector[Double], pattern: Vector[Int], density: Double, hit: Int, phenotype: P, individual: Individual[P])

  def result[P](
    population: Vector[Individual[P]],
    state: EvolutionState[PPSEState],
    continuous: Vector[C],
    pattern: P => Vector[Int]) =

    def computePDF(likelihoodRatioMap: SamplingWeightMap) =
      val totalDensity = likelihoodRatioMap.values.sum
      likelihoodRatioMap.toMap.map: (p, density) =>
        def normalizedDensity = if totalDensity == 0.0 then 0.0 else density / totalDensity
        (p, normalizedDensity)

    val densityMap = computePDF(state.s.likelihoodRatioMap)
    val hitMap = state.s.hitmap

    population.map: i =>
      val pa = pattern(i.phenotype)

      Result(
        scaleContinuousValues(Genome.values(i.genome), continuous).toVector,
        pa,
        densityMap.getOrElse(pa, 0.0),
        hitMap.getOrElse(pa, 0),
        i.phenotype,
        i)


  def result[P](ppse: PPSE[P], population: Vector[Individual[P]], state: EvolutionState[PPSEState]): Vector[Result[P]] =
    result(population, state, ppse.continuous, ppse.pattern)

  case class Individual[P](
    genome: Genome,
    phenotype: P,
    generation: Long,
    initial: Boolean)

  def buildIndividual[P](g: Genome, f: P, generation: Long, initial: Boolean) = Individual(g, f, generation, initial)

  def initialGenomes(number: Int, continuous: Vector[C], reject: Option[IArray[Double] => Boolean], warmupSampler: Int, rng: scala.util.Random) =
    def sample() = (PPSEOperation.randomUnscaledContinuousValues(continuous.size, rng), 1.0)

    def rejectValue(x: IArray[Double], density: Double) = reject.getOrElse(noRejection)(x)
    val sampler = PPSEOperation.toSampler(sample, rejectValue, continuous, rng)

    (0 to number).map: _ =>
      val g = RejectionSampler.sampleNoDensity(sampler)
      Genome(g, None)
    .toVector

  def breeding[P](
    continuous: Vector[C],
    lambda: Int,
    reject: Option[IArray[Double] => Boolean],
    warmupSampler: Int,
    regularisationEpsilon: Double,
    defensiveDistributionWeight: Double,
    density: Option[IArray[Double] => Double]): Breeding[EvolutionState[PPSEState], Individual[P], Genome] =
    PPSEOperation.breeding(
      continuous,
      Genome.apply,
      lambda,
      reject,
      _.s.gmm,
      warmupSampler,
      regularisationEpsilon = regularisationEpsilon,
      defensiveDistributionWeight = defensiveDistributionWeight,
      density = density)

  def elitism[P: CanContainNaN](
    pattern: P => Vector[Int],
    continuous: Vector[C],
    reject: Option[IArray[Double] => Boolean],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    minClusterSize: Int,
    maxRareSample: Int,
    bootstrapGeneration: Int,
    regularisationEpsilon: Double) =
    PPSEOperation.elitism[EvolutionState[PPSEState], Individual[P], P](
      values = i => Genome.toTuple(i.genome),
      phenotype = _.phenotype,
      continuous = continuous,
      reject = reject,
      pattern = pattern,
      likelihoodRatioMap = Focus[EvolutionState[PPSEState]](_.s.likelihoodRatioMap),
      hitmap = Focus[EvolutionState[PPSEState]](_.s.hitmap),
      gmm = Focus[EvolutionState[PPSEState]](_.s.gmm),
      generation = _.generation,
      individualGeneration = Focus[Individual[P]](_.generation),
      iterations = iterations,
      tolerance = tolerance,
      dilation = dilation,
      maxRareSample = maxRareSample,
      minClusterSize = minClusterSize,
      bootstrapGeneration = bootstrapGeneration,
      regularisationEpsilon = regularisationEpsilon)

  def expression[P](phenotype: (util.Random, IArray[Double]) => P, continuous: Vector[C]) = (random: util.Random, genome: Genome, generation: Long, initial: Boolean) =>
    val sc = scaleContinuousValues(Genome.values(genome), continuous)
    Individual(genome, phenotype(random, sc), generation, initial)

  given [P: CanContainNaN]: Algorithm[PPSE[P], Individual[P], Genome, EvolutionState[PPSEState]] with
    def initialState(t: PPSE[P], rng: util.Random) = EvolutionState[PPSEState](s = PPSEState())

    override def initialPopulation(t: PPSE[P], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      noisy.initialPopulation[Genome, Individual[P]](
        PPSE.initialGenomes(t.lambda, t.continuous, t.reject, t.warmupSampler, rng),
        PPSE.expression(t.phenotype, t.continuous),
        rng,
        parallel
      )

    def step(t: PPSE[P]) =
      noisy.step[EvolutionState[PPSEState], Individual[P], Genome](
        PPSE.breeding(t.continuous, t.lambda, t.reject, warmupSampler =  t.warmupSampler, regularisationEpsilon = t.regularisationEpsilon, defensiveDistributionWeight = t.defensiveDistributionWeight, density = t.density),
        PPSE.expression(t.phenotype, t.continuous),
        PPSE.elitism(t.pattern, t.continuous, t.reject, iterations = t.iterations, tolerance = t.tolerance, dilation = t.dilation, minClusterSize = t.minClusterSize, maxRareSample =  t.maxRareSample, bootstrapGeneration = t.bootstrapGeneration, regularisationEpsilon = t.regularisationEpsilon),
        Focus[EvolutionState[PPSEState]](_.generation),
        Focus[EvolutionState[PPSEState]](_.evaluated)
      )


case class PPSE[P](
  lambda: Int,
  phenotype: (util.Random, IArray[Double]) => P,
  pattern: P => Vector[Int],
  continuous: Vector[C],
  reject: Option[IArray[Double] => Boolean] = None,
  density: Option[IArray[Double] => Double] = None,
  iterations: Int = 1000,
  tolerance: Double = 0.001,
  warmupSampler: Int = 1000,
  dilation: Double = 1.5,
  maxRareSample: Int = 10,
  minClusterSize: Int = 5,
  bootstrapGeneration: Int = 10,
  regularisationEpsilon: Double = 10e-6,
  defensiveDistributionWeight: Double = 0.05)

object PPSEOperation:
  def randomUnscaledContinuousValues(genomeLength: Int, rng: scala.util.Random) = IArray.fill(genomeLength)(() => rng.nextDouble()).map(_())

  def toSampler(sample: () => (IArray[Double], Double), reject: (IArray[Double], Double) => Boolean, continuous: Vector[C], rng: Random) =
    def acceptPoint(x: IArray[Double]) = x.forall(_ <= 1.0) && x.forall(_ >= 0.0)

    def acceptFunction(x: IArray[Double], density: Double) =
      def rejectValue = reject(scaleContinuousValues(x, continuous), density)
      acceptPoint(x) && !rejectValue

    new RejectionSampler(sample, acceptFunction)

  def gmmToSampler(gmm: GMM, regularisationEpsilon: Double, reject: (IArray[Double], Double) => Boolean, continuous: Vector[C], q: Double, rng: Random) =
    import mgo.tools.clustering.GMM
    val distribution = GMM.toDistribution(gmm, regularisationEpsilon, rng)

    def sample() =
      val x = defensiveSample(distribution, q, continuous.size, rng)
      val d = defensiveDensity(distribution, q, x)
      (x, d)

    toSampler(sample, reject, continuous, rng)

  def defensiveSample(gmm: MixtureMultivariateNormalDistribution, q: Double, size: Int, rng: Random) =
    if rng.nextDouble() < q
    then randomUnscaledContinuousValues(size, rng)
    else IArray.unsafeFromArray(gmm.sample())

  def defensiveDensity(gmm: MixtureMultivariateNormalDistribution, q: Double, x: IArray[Double]) =
    (1 - q) * gmm.density(x.unsafeToArray) + q

  def breeding[S, I, G](
    continuous: Vector[C],
    buildGenome: ((IArray[Double], Option[Double])) => G,
    lambda: Int,
    reject: Option[IArray[Double] => Boolean],
    gmm: S => Option[GMM],
    warmupSampler: Int,
    regularisationEpsilon: Double,
    defensiveDistributionWeight: Double,
    density: Option[IArray[Double] => Double]): Breeding[S, I, G] =
    (s, population, rng) =>

      def sampleUniform: Vector[G] =
        def sample() = (randomUnscaledContinuousValues(continuous.size, rng), 1.0)
        def rejectValue(x: IArray[Double], density: Double) = reject.getOrElse(noRejection)(x)
        val sampler = toSampler(sample, rejectValue, continuous, rng)
        (0 to lambda).map: _ =>
          val g = RejectionSampler.sampleNoDensity(sampler)
          buildGenome(g, None)
        .toVector

      gmm(s) match
        case None => sampleUniform
        case Some(gmmValue) if gmmValue.isEmpty => sampleUniform
        case Some(gmmValue) =>
          def inverseDensity(x: IArray[Double], densityValue: Double) =
            val xDensity =
              density.map: df =>
                val scaled = scaleContinuousValues(x, continuous)
                df(scaled)
              .getOrElse(1.0)

            xDensity / densityValue

          def rejectValue(x: IArray[Double], density: Double) =
            reject.getOrElse(noRejection)(x) ||
              rejectNaN[IArray[Double]](identity)(x)

          val sampler = gmmToSampler(gmmValue, regularisationEpsilon, rejectValue, continuous, defensiveDistributionWeight, rng)
          val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
          val (_, sampled) = RejectionSampler.sampleArray(sampler, lambda, samplerState)
          val breed =
            sampled.toVector.map: s =>
              val id = inverseDensity(s._1, s._2)
              buildGenome(s._1, Some(id))
          breed

  def elitism[S, I, P: CanContainNaN](
    values: I => (IArray[Double], Option[Double]),
    phenotype: I => P,
    pattern: P => Vector[Int],
    continuous: Vector[C],
    reject: Option[IArray[Double] => Boolean],
    likelihoodRatioMap: monocle.Lens[S, PPSE.SamplingWeightMap],
    hitmap: monocle.Lens[S, HitMap],
    gmm: monocle.Lens[S, Option[GMM]],
    generation: S => Long,
    individualGeneration: monocle.Lens[I, Long],
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    maxRareSample: Int,
    minClusterSize: Int,
    bootstrapGeneration: Int,
    regularisationEpsilon: Double): Elitism[S, I] =  (state, population, candidates, rng) =>

    val offSpringWithNoNan = filterNaN(candidates, phenotype)

    def keepRandom(i: Vector[I]): Vector[I] =
      if i.isEmpty
      then i
      else
        val first = i.map(individualGeneration.get).min
        val selected = i(rng.nextInt(i.size))
        Vector(individualGeneration.set(first)(selected))

    val newPopulation = keepNiches(phenotype andThen pattern, keepRandom)(population ++ offSpringWithNoNan)

    def updatedState =
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
          continuous = continuous,
          random = rng)

      def bootstrapState = gmm.modify(gmm => elitedGMM orElse gmm)(state)
      def evolutionState =
        (gmm.modify(gmm => elitedGMM orElse gmm) andThen
          likelihoodRatioMap.replace(elitedDensity) andThen
          hitmap.replace(elitedHitMap))(state)


      if generation(state) < bootstrapGeneration
      then bootstrapState
      else evolutionState

    (updatedState, newPopulation)


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

    def fittedGMM =
      if rareIndividuals.length < minClusterSize
      then GMM.empty
      else
        fitGMM(
          rareIndividuals,
          regularisationEpsilon = regularisationEpsilon,
          iterations = iterations,
          tolerance = tolerance,
          minClusterSize = minClusterSize)

    def gmmWithOutliers = EMGMM.integrateOutliers(rareIndividuals, fittedGMM, regularisationEpsilon)

    if rareIndividuals.isEmpty
    then None
    else Some(GMM.dilate(gmmWithOutliers, dilation))



  def updateState(
    genomes: Array[Array[Double]],
    patterns: Array[Array[Int]],
    offspringGenomes: Array[(IArray[Double], Option[Double])],
    offspringPatterns: Array[Array[Int]],
    likelihoodRatioMap: PPSE.SamplingWeightMap,
    hitMap: HitMap,
    maxRareSample: Int,
    regularisationEpsilon: Double,
    iterations: Int,
    tolerance: Double,
    dilation: Double,
    minClusterSize: Int,
    continuous: Vector[C],
    random: Random): (HitMap, PPSE.SamplingWeightMap, Option[GMM]) =

    def newHitMap =
      def updateHits(m: HitMap, p: Vector[Int]) = m.updatedWith(p)(v => Some(v.getOrElse(0) + 1))
      (offspringGenomes zip offspringPatterns).foldLeft(hitMap): (m, gp) =>
        val (g, p) = gp
        if g._2.isDefined
        then updateHits(m, p.toVector)
        else m

    def newLikelihoodRatioMap =
      def offSpringDensities =
        val groupedGenomes = (offspringGenomes zip offspringPatterns).groupMap(_._2)(_._1)
        groupedGenomes.view.mapValues: v =>
          v.map:
            case (genome, Some(inverseDensity)) => inverseDensity
            case (genome, None) => 0.0
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


  def fitGMM(
    points: Array[Array[Double]],
    regularisationEpsilon: Double,
    iterations: Int,
    tolerance: Double,
    minClusterSize: Int) =
    def clusterize(x: Array[Array[Double]], minPoints: Int) =
      def covariance(x: Array[Array[Double]]) =
        import org.apache.commons.math3.stat.correlation.Covariance
        new Covariance(x).getCovarianceMatrix.getData

      def computeCentroid(points: Array[Array[Double]]) =
        points.transpose.map(x => x.sum / x.length)

      HDBScan.clusterize(x, minPoints).map: clusters =>
        val centroids = clusters.map(computeCentroid)
        val covariances = clusters.map(covariance)
        (centroids, covariances)

    clusterize(points, minClusterSize) match
      case Some((clusterMeans, clusterCovariances)) =>
        val clusterWeights = Array.fill(clusterMeans.length)(1.0 / clusterMeans.length)

        EMGMM.fit(
          components = clusterMeans.length,
          iterations = iterations,
          tolerance = tolerance,
          x = points,
          means = clusterMeans,
          covariances = clusterCovariances,
          weights = clusterWeights,
          regularisationEpsilon = regularisationEpsilon)._1
      case None => GMM.empty
