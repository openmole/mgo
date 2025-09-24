package mgo.abc

//package mgo.evolution.algorithm
//
///*
// * Copyright (C) 2025 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Affero General Public License for more details.
// *
// * You should have received a copy of the GNU Affero General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//import mgo.tools
//import mgo.evolution.algorithm.*
//import mgo.evolution.*
//import mgo.evolution.algorithm.GenomeVectorDouble.*
//import mgo.evolution.breeding.*
//import mgo.evolution.elitism.*
//import mgo.evolution.ranking.*
//import mgo.tools.*
//import mgo.tools.execution.*
//
//import monocle.*
//import monocle.syntax.all.*
//
//
//import mgo.tools.clustering.GMM
//
//object NewAPMC:
//
//  case class NewAPMCState(gmm: Option[GMM] = None)
//
//  type Genome = (IArray[Double], Double)
//
//  case class Individual(
//    genome: Genome,
//    fitness: Double,
//    generation: Long,
//    initial: Boolean)
//
//  case class Result(continuous: Vector[Double], fitness: Double, individual: Individual)
//
//  def result(population: Vector[Individual], continuous: Vector[C], keepAll: Boolean): Vector[Result] =
//    val individuals =
//      if keepAll
//      then population
//      else
//        if population.nonEmpty
//        then
//          val best = population.minBy(_.fitness).fitness
//          population.takeWhile(_.fitness <= best)
//        else population
//
//    individuals.map: i =>
//      Result(scaleContinuousValues(i.genome._1, continuous).toVector, i.fitness, i)
//
//  def result(algo: NewAPMC, population: Vector[Individual], keepAll: Boolean = false): Vector[Result] =
//    result(population, algo.continuous, keepAll = keepAll)
//
//
//  def buildIndividual(g: Genome, f: Double, generation: Long, initial: Boolean) = Individual(g, f, generation, initial)
//
//  def initialGenomes(number: Int, continuous: Vector[C], reject: Option[IArray[Double] => Boolean], warmupSampler: Int, rng: scala.util.Random) =
//    def sample() = (NewAPMCOperation.randomUnscaledContinuousValues(continuous.size, rng), Lazy(1.0))
//
//    val sampler = PPSEOperation.toSampler(sample, reject, continuous, rng)
//    val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
//
//    (0 to number).map: _ =>
//      val (g, d) = sample()
//      (g, d.value)
//    .toVector
//
//
//  def breeding(
//    continuous: Vector[C],
//    lambda: Int,
//    reject: Option[IArray[Double] => Boolean],
//    warmupSampler: Int): Breeding[EvolutionState[NewAPMCState], Individual, Genome] =
//    NewAPMCOperation.breeding(
//      continuous,
//      identity,
//      lambda,
//      reject,
//      _.s.gmm,
//      warmupSampler)
//
//  def elitism(
//    mu: Int,
//    continuous: Vector[C],
//    reject: Option[IArray[Double] => Boolean],
//    density: Option[IArray[Double] => Double]) =
//    NewAPMCOperation.elitism[EvolutionState[NewAPMCState], Individual](
//      mu = mu,
//      fitness = _.fitness,
//      values = _.genome,
//      continuous = continuous,
//      reject = reject,
//      gmm = Focus[EvolutionState[NewAPMCState]](_.s.gmm),
//      density = density)
//
//  def expression(fitness: IArray[Double] => Double, continuous: Vector[C]) = (genome: Genome, generation: Long, initial: Boolean) =>
//    val sc = scaleContinuousValues(genome._1, continuous)
//    Individual(genome, fitness(sc), generation, initial)
//
//  given [P: CanContainNaN]: Algorithm[NewAPMC, Individual, Genome, EvolutionState[NewAPMCState]] with
//    override def initialState(t: NewAPMC, rng: util.Random) = EvolutionState[NewAPMCState](s = NewAPMCState())
//
//    override def initialPopulation(t: NewAPMC, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
//      deterministic.initialPopulation[Genome, Individual](
//        NewAPMC.initialGenomes(t.lambda, t.continuous, t.reject, t.warmupSampler, rng),
//        NewAPMC.expression(t.fitness, t.continuous), parallel)
//
//    override def step(t: NewAPMC) =
//      deterministic.step[EvolutionState[NewAPMCState], Individual, Genome](
//        NewAPMC.breeding(t.continuous, t.lambda, t.reject, t.warmupSampler),
//        NewAPMC.expression(t.fitness, t.continuous),
//        NewAPMC.elitism(t.mu, t.continuous, t.reject, t.density),
//        Focus[EvolutionState[NewAPMCState]](_.generation),
//        Focus[EvolutionState[NewAPMCState]](_.evaluated)
//      )
//
//case class NewAPMC(
//  mu: Int,
//  lambda: Int,
//  fitness: IArray[Double] => Double,
//  continuous: Vector[C],
//  reject: Option[IArray[Double] => Boolean] = None,
//  density: Option[IArray[Double] => Double] = None,
//  warmupSampler: Int = 10000)
//
//
//object NewAPMCOperation:
//  export PPSEOperation.{randomUnscaledContinuousValues, toSampler, gmmToSampler}
//
//  def breeding[S, I, G](
//    continuous: Vector[C],
//    buildGenome: ((IArray[Double], Double)) => G,
//    lambda: Int,
//    reject: Option[IArray[Double] => Boolean],
//    gmm: S => Option[GMM],
//    warmupSampler: Int): Breeding[S, I, G] =
//    (s, population, rng) =>
//      gmm(s) match
//        case None =>
//          def sample() = (randomUnscaledContinuousValues(continuous.size, rng), Lazy(1.0))
//          val sampler = toSampler(sample, reject, continuous, rng)
//          val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
//          (0 to lambda).map: _ =>
//            val (g, d) = sample()
//            buildGenome(g, d.value)
//          .toVector
//        case Some(gmmValue) =>
//          val sampler = gmmToSampler(gmmValue, reject, continuous, rng)
//          val samplerState = RejectionSampler.warmup(sampler, warmupSampler)
//          val (_, sampled) = RejectionSampler.sampleArray(sampler, lambda, samplerState)
//          val breed = sampled.toVector.map(s => buildGenome(s._1, s._2))
//          breed
//
//
//  def elitism[S, I](
//    values: I => (IArray[Double], Double),
//    fitness: I => Double,
//    mu: Int,
//    continuous: Vector[C],
//    reject: Option[IArray[Double] => Boolean],
//    gmm: monocle.Lens[S, Option[GMM]],
//    density: Option[IArray[Double] => Double]): Elitism[S, I] =  (state, population, candidates, rng) =>
//
//    val p1 =
//      val newPopulation = population ++ candidates
//      if newPopulation.size < mu
//      then newPopulation
//      else newPopulation.sortBy(fitness).take(mu)
//
//    val means = p1.map(x => values(x)._1.toArray).toArray
//
//    val weights =
//      p1.map: p =>
//        val (v, d) = values(p)
//        val num = density.map(d => d(v)).getOrElse(1.0)
//        num / d
//      .toArray
//
//    val newGMM = buildGMM(means, weights)
//    (gmm.replace(Some(newGMM))(state), p1)
//
//
//  import org.apache.commons.math3.distribution._
//  import org.apache.commons.math3.linear._
//  import scala.util.Random
//  import scala.jdk.CollectionConverters.*
//
//  /** Compute weighted mean */
//  def weightedMean(thetas: Array[Array[Double]], weights: Array[Double]): Array[Double] =
//    val d = thetas(0).length
//    val mu = Array.fill(d)(0.0)
//    val wSum = weights.sum
//    for i <- thetas.indices do
//      for j <- 0 until d do
//        mu(j) += weights(i) * thetas(i)(j)
//    for j <- 0 until d do mu(j) /= wSum
//    mu
//
//  /** Compute weighted covariance */
//  def weightedCovariance(thetas: Array[Array[Double]], weights: Array[Double]): Array[Array[Double]] =
//    val d = thetas(0).length
//    val mu = weightedMean(thetas, weights)
//    val wSum = weights.sum
//
//    val cov = Array.ofDim[Double](d, d)
//
//    for i <- thetas.indices do
//      val diff = thetas(i).zip(mu).map((x, m) => x - m)
//      for a <- 0 until d do
//        for b <- 0 until d do
//          cov(a)(b) += weights(i) * diff(a) * diff(b)
//
//    for a <- 0 until d do
//      for b <- 0 until d do
//        cov(a)(b) /= wSum
//
//    cov
//
//  /** Build Gaussian Mixture Model from particles */
//  def buildGMM(
//    thetas: Array[Array[Double]],
//    weights: Array[Double]): GMM =
//    val cov = weightedCovariance(thetas, weights)
//    val propCov = cov.map(_.map(_ * 2.0)) // scale by 2
//
//    // Each component: mean = theta, cov = propCov
//    val components =
//      thetas.indices.map: i =>
//        val mean = thetas(i)
//        GMM.Component(mean, propCov, weights(i))
//
//    GMM(components)
