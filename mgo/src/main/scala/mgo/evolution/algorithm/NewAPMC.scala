package mgo.evolution.algorithm

/*
 * Copyright (C) 2025 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import mgo.tools
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


import mgo.tools.clustering.GMM

object NewAPMC:

  case class APMCState()

object NewAPMCOperation:
  export PPSEOperation.{randomUnscaledContinuousValues, toSampler, gmmToSampler}

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
    distance: P => Double,
    mu: Int,
    continuous: Vector[C],
    reject: Option[IArray[Double] => Boolean],
    gmm: monocle.Lens[S, Option[GMM]],
    epsilon: monocle.Lens[S, Double],
    regularisationEpsilon: Double,
    density: Option[IArray[Double] => Double]): Elitism[S, I] =  (state, population, candidates, rng) =>

    val p1 =
      val newPopulation = population ++ candidates
      if newPopulation.size < mu
      then newPopulation
      else newPopulation.sortBy(phenotype andThen distance).take(mu)

    val means = p1.map(x => values(x)._1.toArray).toArray

    val weights =
      p1.map: p =>
        val (v, d) = values(p)
        val num = density.map(d => d(v)).getOrElse(1.0)
        num / d
      .toArray

    val newGMM = buildGMM(means, weights)
    (gmm.replace(Some(newGMM))(state), p1)


  import org.apache.commons.math3.distribution._
  import org.apache.commons.math3.linear._
  import scala.util.Random
  import scala.jdk.CollectionConverters.*

  /** Compute weighted mean */
  def weightedMean(thetas: Array[Array[Double]], weights: Array[Double]): Array[Double] =
    val d = thetas(0).length
    val mu = Array.fill(d)(0.0)
    val wSum = weights.sum
    for i <- thetas.indices do
      for j <- 0 until d do
        mu(j) += weights(i) * thetas(i)(j)
    for j <- 0 until d do mu(j) /= wSum
    mu

  /** Compute weighted covariance */
  def weightedCovariance(thetas: Array[Array[Double]], weights: Array[Double]): Array[Array[Double]] =
    val d = thetas(0).length
    val mu = weightedMean(thetas, weights)
    val wSum = weights.sum

    val cov = Array.ofDim[Double](d, d)

    for i <- thetas.indices do
      val diff = thetas(i).zip(mu).map((x, m) => x - m)
      for a <- 0 until d do
        for b <- 0 until d do
          cov(a)(b) += weights(i) * diff(a) * diff(b)

    for a <- 0 until d do
      for b <- 0 until d do
        cov(a)(b) /= wSum

    cov

  /** Build Gaussian Mixture Model from particles */
  def buildGMM(
    thetas: Array[Array[Double]],
    weights: Array[Double]): GMM =
    val cov = weightedCovariance(thetas, weights)
    val propCov = cov.map(_.map(_ * 2.0)) // scale by 2

    // Each component: mean = theta, cov = propCov
    val components =
      thetas.indices.map: i =>
        val mean = thetas(i)
        GMM.Component(mean, propCov, weights(i))

    GMM(components)
