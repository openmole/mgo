/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package mgo.evolution.algorithm

import mgo.evolution.*
import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.evolution.ranking.*
import monocle.Focus
import monocle.syntax.all.*
import mgo.tools.*


object OnePlusOneCMAES:

  type Genome = IArray[Double]
  case class Individual(genome: Genome, fitness: Double, generation: Long, initial: Boolean)
  type State = EvolutionState[OnePlusOneCMAESOperation.A]

  case class Result(continuous: Vector[Double], fitness: Double, individual: Individual)

  def initialGenomes(continuous: Vector[C], rng: scala.util.Random): Vector[Genome] =
    Vector:
      IArray.from:
        continuous.map: c =>
          rng.nextDouble()

  def initialState(continuous: Vector[C]) : State =
    val parameters = OnePlusOneCMAESOperation.Parameters(continuous.size)
    val a =
      OnePlusOneCMAESOperation.A(
        parameters.p_targetSucc,
        parameters.sigma,
        IArray.fill(continuous.size)(0.0),
        IArray.tabulate(continuous.size, continuous.size): (i, j) =>
          if i == j then 1.0 else 0.0
      )
    EvolutionState(s = a)

  def breeding: Breeding[State, Individual, Genome] =
    OnePlusOneCMAESOperation.breeding[State, Individual, Genome](
      _.genome,
      _.s,
      identity)

  def expression(express: IArray[Double] => Double, continuous: Vector[C]): (Genome, Long, Boolean) => Individual =
    (ge, g, i) =>
      Individual(ge, express(scaleContinuousValues(ge, continuous)), g, i)

  def elitism(continuous: Vector[C]): Elitism[State, Individual] =
    val parameters = OnePlusOneCMAESOperation.Parameters(continuous.size)
    OnePlusOneCMAESOperation.elitism[State, Individual](
      parameters,
      _.genome,
      _.fitness,
      Focus[State](_.s)
    )

  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] =
    population.map: i =>
      Result(scaleContinuousVectorValues(i.genome.toVector, continuous), i.fitness, i)

  def result(t: OnePlusOneCMAES, population: Vector[Individual]): Vector[Result] =
    result(population, t.continuous)


  given isAlgorithm: Algorithm[OnePlusOneCMAES, Individual, Genome, State] with
      override def initialState(t: OnePlusOneCMAES, rng: scala.util.Random) =
        OnePlusOneCMAES.initialState(t.continuous)

      override def initialPopulation(t: OnePlusOneCMAES, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
        deterministic.initialPopulation[Genome, Individual](
          OnePlusOneCMAES.initialGenomes(t.continuous, rng),
          OnePlusOneCMAES.expression(t.fitness, t.continuous),
          parallel)

      override def step(t: OnePlusOneCMAES) =
        deterministic.step[State, Individual, Genome](
          OnePlusOneCMAES.breeding,
          OnePlusOneCMAES.expression(t.fitness, t.continuous),
          OnePlusOneCMAES.elitism(t.continuous),
          Focus[State](_.generation),
          Focus[State](_.evaluated))

case class OnePlusOneCMAES(
  fitness: IArray[Double] => Double,
  continuous: Vector[C] = Vector.empty)

// Implementation of: https://ieeexplore.ieee.org/document/6792721
object OnePlusOneCMAESOperation:

  def breeding[S, I, G](
    values: I => IArray[Double],
    a: S => A,
    buildGenome: IArray[Double] => G): Breeding[S, I, G] =
    (s, population, rng) =>
      import org.apache.commons.math3.distribution.*
      import org.apache.commons.math3.linear.*

      assert(population.size == 1)

      val i = population.head
      val aValue = a(s)

      val newX =
        util.Try:
          val cov =
            Array2DRowRealMatrix(aValue.C.map(_.unsafeArray).unsafeArray).
              scalarMultiply(Math.pow(aValue.sigma, 2.0))

          val distribution =
            MultivariateNormalDistribution(
              apacheRandom(rng),
              values(i).unsafeArray,
              cov.getData
            )

          rejectSampleUnitSquare: () =>
            val s = distribution.sample()
            if s.exists(_.isNaN) then throw RuntimeException("Sample contains NaN")
            IArray.unsafeFromArray(s)

        .getOrElse:
          val cov =
            MatrixUtils.createRealIdentityMatrix(values(i).length).
              scalarMultiply(Math.pow(aValue.sigma, 2.0))

          val distribution =
            MultivariateNormalDistribution(
              apacheRandom(rng),
              values(i).unsafeArray,
              cov.getData
            )

          rejectSampleUnitSquare: () =>
            IArray.unsafeFromArray(distribution.sample())

      Vector(buildGenome(newX))

  def elitism[S, I](
    parameters: Parameters,
    values: I => IArray[Double],
    fitness: I => Double,
    a: monocle.Lens[S, A]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      assert(population.size == 1)
      assert(candidates.size == 1)
      val i_g = population.head
      val i_gp1 = candidates.head

      val success = fitness(i_gp1) <= fitness(i_g)

      val stepUpdated =
        updateStepSize(a.get(s), if success then 1.0 else 0.0, parameters)

      if success
      then
        val sigma_g = a.get(s).sigma
        val x_step = (values(i_gp1) zip values(i_g)).map: (x_gp1, x_g) =>
          (x_gp1 - x_g) / sigma_g

        val covUpdated = updateCovariance(stepUpdated, x_step, parameters)

        val newS = a.replace(covUpdated)(s)
        (newS, candidates)

      else (a.replace(stepUpdated)(s), population)

  object Parameters:
    def apply(n: Int) =
      val p_TargetSucc = 1.0 / (5.0 + 1.0 / 2.0)
      new Parameters(
        sigma = 1.0 / 6.0,
        d = 1 + math.floor(n / 2.0),
        p_targetSucc = p_TargetSucc,
        c_p = p_TargetSucc / (2.0 + p_TargetSucc),
        c_c = 2.0 / (n + 2.0),
        c_cov = 2.0 / (Math.pow(n, 2.0) + 6.0),
        p_thresh = 0.44
      )

  case class Parameters(
    sigma: Double,
    d: Double,
    p_targetSucc: Double,
    c_p: Double,
    c_c: Double,
    c_cov: Double,
    p_thresh: Double,
    epsilon: Double = 1e-12,
    sigmaMax: Double = 0.5)

  case class A(pSuccBar: Double, sigma: Double, pc: IArray[Double], C: IArray[IArray[Double]])

  def updateStepSize(a: A, pSucc: Double, parameters: Parameters) =
    import parameters.*

    val newPSuccBar = (1.0 - parameters.c_p) * a.pSuccBar + c_p * pSucc
    val newSigma = a.sigma * Math.exp((1.0 / d) * (newPSuccBar - p_targetSucc) / (1.0 - p_targetSucc))
    a.copy(
      pSuccBar = newPSuccBar,
      sigma = Math.min(Math.max(newSigma, epsilon), sigmaMax)
    )

  def updateCovariance(a: A, x_step: IArray[Double], parameters: Parameters): A =
    import parameters.*
    import org.apache.commons.math3.linear.*

    def regularizePD(m: RealMatrix): RealMatrix =
      val sym = m.add(m.transpose()).scalarMultiply(0.5)
      val n = sym.getColumnDimension
      val I = MatrixUtils.createRealIdentityMatrix(n)
      val diagMin = (0 until n).map(i => sym.getEntry(i, i)).min
      val eps = math.max(0.0, epsilon - diagMin)
      sym.add(I.scalarMultiply(eps))

    lazy val cMatrix = Array2DRowRealMatrix(a.C.map(_.unsafeArray).unsafeArray)

    if a.pSuccBar < p_thresh
    then
      val newpc =
        val lhs = a.pc.map(_ * (1.0 - c_c))

        val rhs =
          val f = Math.sqrt(c_c * (2.0 - c_c))
          x_step.map(_ * f)

        (lhs zip rhs).map(_ + _)

      val newC =
        val lhs = cMatrix.scalarMultiply(1.0 - c_cov)

        val rhs =
          val pcVector = ArrayRealVector(newpc.toArray)
          pcVector.outerProduct(pcVector).scalarMultiply(c_cov)

        IArray.unsafeFromArray(
          regularizePD(lhs.add(rhs)).getData.map(IArray.unsafeFromArray)
        )

      a.copy(pc = newpc, C = newC)
    else
      val newpc = a.pc.map(_ * (1.0 - c_c))

      val newC =
        val lhs = cMatrix.scalarMultiply(1.0 - c_cov)

        val rhs =
          val pcpct =
            val pcVector = ArrayRealVector(newpc.toArray)
            pcVector.outerProduct(pcVector)

          val ccmul = cMatrix.scalarMultiply(c_c * (2.0 - c_c))

          pcpct.add(ccmul).scalarMultiply(c_cov)

        IArray.unsafeFromArray(
          regularizePD(lhs.add(rhs)).getData.map(IArray.unsafeFromArray)
        )

      a.copy(pc = newpc, C = newC)
