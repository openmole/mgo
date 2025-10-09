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
import mgo.evolution.dominance.*
import mgo.evolution.algorithm.GenomeVectorDouble.*
import mgo.evolution.breeding.*
import mgo.evolution.elitism.*
import mgo.evolution.ranking.*
import monocle.Focus
import monocle.syntax.all.*
import mgo.tools.*

object MOCMAES:
  case class Genome(x: IArray[Double], a: OnePlusOneCMAESOperation.A, ancestor: Option[MOCMEASOperation.Ancestor])
  case class Individual(genome: Genome, fitness: IArray[Double], generation: Long, initial: Boolean)
  type State = EvolutionState[Unit]

  case class Result(continuous: Vector[Double], fitness: Vector[Double], individual: Individual)

  def initialGenomes(parameters: OnePlusOneCMAESOperation.Parameters, lambda: Int, continuous: Vector[C], rng: scala.util.Random): Vector[Genome] =
    Vector.fill(lambda):
      val x =
        IArray.from:
          continuous.map: c =>
            rng.nextDouble()

      val a = OnePlusOneCMAESOperation.initialA(parameters, continuous.size)
      Genome(x, a, None)

  def initialState = EvolutionState[Unit](s = ())

  def breeding(parameters: OnePlusOneCMAESOperation.Parameters, lambda: Int, genomeDiversity: Boolean): Breeding[State, Individual, Genome] =
    MOCMEASOperation.breeding[State, Individual, Genome](
      parameters,
      lambda,
      _.genome.x,
      _.fitness.toVector,
      _.genome.a,
      (g, a, ancestor) => Genome(g, a, Some(ancestor)),
      genomeDiversity = genomeDiversity
    )

  def elitism(parameters: OnePlusOneCMAESOperation.Parameters, mu: Int, continuous: Vector[C], genomeDiversity: Boolean): Elitism[State, Individual] =
    MOCMEASOperation.elitism[State, Individual](
      parameters,
      mu,
      _.genome.x,
      _.fitness.toVector,
      Focus[Individual](_.genome.ancestor),
      Focus[Individual](_.genome.a),
      genomeDiversity = genomeDiversity
    )

  def expression(express: IArray[Double] => Vector[Double], continuous: Vector[C]): (Genome, Long, Boolean) => Individual =
    (ge, g, i) =>
      Individual(ge, IArray.from(express(scaleContinuousValues(ge.x, continuous))), g, i)

  def result(t: MOCMAES, population: Vector[Individual]): Vector[Result] =
    result(population, t.continuous)

  def result(population: Vector[Individual], continuous: Vector[C]): Vector[Result] =
    population.map: i =>
      Result(scaleContinuousVectorValues(i.genome.x.toVector, continuous), i.fitness.toVector, i)

  given isAlgorithm: Algorithm[MOCMAES, Individual, Genome, State] with
    override def initialState(t: MOCMAES, rng: scala.util.Random) = MOCMAES.initialState

    override def initialPopulation(t: MOCMAES, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual](
        MOCMAES.initialGenomes(t.parameters, t.lambda, t.continuous, rng),
        MOCMAES.expression(t.fitness, t.continuous),
        parallel)

    override def step(t: MOCMAES) =
      deterministic.step[State, Individual, Genome](
        MOCMAES.breeding(t.parameters, t.lambda, t.genomeDiversity),
        MOCMAES.expression(t.fitness, t.continuous),
        MOCMAES.elitism(t.parameters, t.mu, t.continuous, t.genomeDiversity),
        Focus[State](_.generation),
        Focus[State](_.evaluated)
      )

  def apply(
   mu: Int,
   lambda: Int,
   fitness: IArray[Double] => Vector[Double],
   continuous: Vector[C],
   genomeDiversity: Boolean = false) =
    new MOCMAES(
      mu,
      lambda,
      fitness,
      continuous,
      OnePlusOneCMAESOperation.Parameters(continuous.size),
      genomeDiversity = genomeDiversity
    )


case class MOCMAES(
  mu: Int,
  lambda: Int,
  fitness: IArray[Double] => Vector[Double],
  continuous: Vector[C],
  parameters: OnePlusOneCMAESOperation.Parameters,
  genomeDiversity: Boolean)


object MOCMEASOperation:
  import OnePlusOneCMAESOperation.{A, Parameters}

  type Fitness = Vector[Double]
  case class Ancestor(x: IArray[Double], fitness: Fitness)

  def sameKernel(x1: IArray[Double], a1: A, x2: IArray[Double], a2: A) =
    (x1 sameElements x2) &&
      a1.sigma == a2.sigma &&
      (a1.C zip a2.C).forall((a, b) => (a sameElements b))

  def breeding[S, I, G](
    parameters: Parameters,
    lambda: Int,
    values: I => IArray[Double],
    fitness: I => Fitness,
    a: I => A,
    buildGenome: (IArray[Double], A, Ancestor) => G,
    genomeDiversity: Boolean): Breeding[S, I, G] =
    (s, population, rng) =>

      val ranks =
        if !genomeDiversity
        then paretoRankingMinAndCrowdingDiversity(population, fitness)
        else paretoRankingMinAndCrowdingDiversityWithGenomeDiversity(population, fitness, i => (values(i), IArray.empty))

      def draw =
        val selected = tournament(ranks, _ => 1)(s, population, rng)
        val sampled = OnePlusOneCMAESOperation.breeding[A, I, IArray[Double]](parameters, values, identity, identity)(a(selected), Vector(selected), rng).head
        buildGenome(sampled, a(selected), Ancestor(values(selected), fitness(selected)))

      Vector.fill(lambda)(draw)

  def elitism[S, I](
    parameters: Parameters,
    mu: Int,
    x: I => IArray[Double],
    fitness: I => Vector[Double],
    ancestor: monocle.Lens[I, Option[Ancestor]],
    a: monocle.Lens[I, A],
    genomeDiversity: Boolean): Elitism[S, I] =
    (s, population, candidates, rng) =>

      def processCandidate(population: Vector[I], candidate: I) =
        ancestor.get(candidate) match
          case None => population ++ Vector(candidate)
          case Some(cAncestor) =>
            val from =
              population.indexWhere: p =>
                sameKernel(cAncestor.x, a.get(candidate), x(p), a.get(p))

            val success = !nonStrictDominance.isDominated(fitness(candidate), cAncestor.fitness)

            val updatedCandidate =
              val stepUpdated = OnePlusOneCMAESOperation.updateStepSize(a.get(candidate), if success then 1.0 else 0.0, parameters)

              val sigma_g = a.get(candidate).sigma
              val x_step =
                (x(candidate) zip cAncestor.x).map: (x_gp1, x_g) =>
                  (x_gp1 - x_g) / sigma_g

              val covUpdated = OnePlusOneCMAESOperation.updateCovariance(stepUpdated, x_step, parameters)

              a.replace(covUpdated)(candidate)

            from match
              case -1 => population ++ Vector(updatedCandidate)
              case index =>
                val updatedI =
                  val existingI = population(index)
                  val stepUpdated = OnePlusOneCMAESOperation.updateStepSize(a.get(existingI), if success then 1.0 else 0.0, parameters)
                  a.replace(stepUpdated)(existingI)

                population.patch(index, Seq(updatedI), 1) ++ Vector(updatedCandidate)

      val updatedPopulation = candidates.foldLeft(population)(processCandidate)

      val ranks =
        if !genomeDiversity
        then paretoRankingMinAndCrowdingDiversity(updatedPopulation, fitness)
        else paretoRankingMinAndCrowdingDiversityWithGenomeDiversity(updatedPopulation, fitness, i => (x(i), IArray.empty))

      val elitePopulation = keepHighestRanked(updatedPopulation, ranks, mu)

      (s, elitePopulation.map(ancestor.replace(None)))


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

  def initialState(parameters: OnePlusOneCMAESOperation.Parameters, continuous: Vector[C]) : State =
    val a =
      OnePlusOneCMAESOperation.A(
        parameters.p_targetSucc,
        parameters.sigma,
        IArray.fill(continuous.size)(0.0),
        IArray.tabulate(continuous.size, continuous.size): (i, j) =>
          if i == j then 1.0 else 0.0
      )
    EvolutionState(s = a)

  def breeding(parameters: OnePlusOneCMAESOperation.Parameters): Breeding[State, Individual, Genome] =
    OnePlusOneCMAESOperation.breeding[State, Individual, Genome](
      parameters,
      _.genome,
      _.s,
      identity)

  def expression(express: IArray[Double] => Double, continuous: Vector[C]): (Genome, Long, Boolean) => Individual =
    (ge, g, i) =>
      Individual(ge, express(scaleContinuousValues(ge, continuous)), g, i)

  def elitism(parameters: OnePlusOneCMAESOperation.Parameters, continuous: Vector[C]): Elitism[State, Individual] =
    OnePlusOneCMAESOperation.elitism[State, Individual](
      parameters,
      Focus[Individual](_.genome),
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
        OnePlusOneCMAES.initialState(t.parameters, t.continuous)

      override def initialPopulation(t: OnePlusOneCMAES, rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
        deterministic.initialPopulation[Genome, Individual](
          OnePlusOneCMAES.initialGenomes(t.continuous, rng),
          OnePlusOneCMAES.expression(t.fitness, t.continuous),
          parallel)

      override def step(t: OnePlusOneCMAES) =
        deterministic.step[State, Individual, Genome](
          OnePlusOneCMAES.breeding(t.parameters),
          OnePlusOneCMAES.expression(t.fitness, t.continuous),
          OnePlusOneCMAES.elitism(t.parameters, t.continuous),
          Focus[State](_.generation),
          Focus[State](_.evaluated))


  def apply(
    fitness: IArray[Double] => Double,
    continuous: Vector[C]) =
    new OnePlusOneCMAES(
      fitness,
      continuous,
      OnePlusOneCMAESOperation.Parameters(continuous.size)
    )

case class OnePlusOneCMAES(
  fitness: IArray[Double] => Double,
  continuous: Vector[C],
  parameters: OnePlusOneCMAESOperation.Parameters)

// Implementation of: https://ieeexplore.ieee.org/document/6792721
object OnePlusOneCMAESOperation:

  def breeding[S, I, G](
    parameters: Parameters,
    values: I => IArray[Double],
    a: S => A,
    buildGenome: IArray[Double] => G): Breeding[S, I, G] =
    (s, population, rng) =>
      assert(population.size == 1)

      val i = population.head
      val iValues = values(i)
      val aValue = a(s)

      val newX =
        val aValue = a(s)
        val step = sampleStep(parameters, aValue.C, aValue.sigma, iValues.length, rng)
        (iValues zip step).map(_ + _).map(x => clamp(x))

      Vector(buildGenome(newX))

  def elitism[S, I](
    parameters: Parameters,
    values: monocle.Lens[I, IArray[Double]],
    fitness: I => Double,
    a: monocle.Lens[S, A]): Elitism[S, I] =
    (s, population, candidates, rng) =>
      assert(candidates.size == 1)
      if population.nonEmpty
      then
        assert(population.size == 1)
        val i_g = population.head
        val i_gp1 = candidates.head

        val success = fitness(i_gp1) <= fitness(i_g)
        val newA = updateA(a.get(s), values.get(i_g), values.get(i_gp1), success, parameters)

//        if newA.pSuccBar < 0.0001
//        then
//          val dim = values.get(i_gp1).length
//          val restartedPop = population.map(values.replace(IArray.fill(dim)(rng.nextDouble())))
//          (a.replace(initialA(parameters, dim))(s), restartedPop)
//        else
        if success
        then (a.replace(newA)(s), candidates)
        else (a.replace(newA)(s), population)

      else (s, candidates)



  object Parameters:
    def apply(n: Int) =
      val p_TargetSucc = 1.0 / (5.0 + 1.0 / 2.0)
      new Parameters(
        sigma = 0.5, //1.0 / 6.0,
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
    sigmaMax: Double = 2.0)

  case class A(pSuccBar: Double, sigma: Double, pc: IArray[Double], C: IArray[IArray[Double]])

  def initialA(parameters: Parameters, dim: Int) =
    OnePlusOneCMAESOperation.A(
      parameters.p_targetSucc,
      parameters.sigma,
      IArray.fill(dim)(0.0),
      IArray.tabulate(dim, dim): (i, j) =>
        if i == j then 1.0 else 0.0
    )

  def updateA(a: A, i_g: IArray[Double], i_gp1: IArray[Double], success: Boolean, parameters: Parameters): A =
    val stepUpdated = updateStepSize(a, if success then 1.0 else 0.0, parameters)

    if success
    then
      val sigma_g = a.sigma
      val x_step =
        (i_gp1 zip i_g).map: (x_gp1, x_g) =>
          (x_gp1 - x_g) / sigma_g

      updateCovariance(stepUpdated, x_step, parameters)
    else stepUpdated

  def sampleStep(parameters: Parameters,C: IArray[IArray[Double]], sigma: Double, dim: Int, rng: util.Random) =
    import org.apache.commons.math3.linear.*
    import org.apache.commons.math3.distribution.*

    util.Try:
      def regularizePD(m: RealMatrix): RealMatrix =
        val sym = m.add(m.transpose()).scalarMultiply(0.5)
        val n = sym.getColumnDimension
        val I = MatrixUtils.createRealIdentityMatrix(n)
        val diagMin = (0 until n).map(i => sym.getEntry(i, i)).min
        val eps = math.max(0.0, parameters.epsilon - diagMin)
        sym.add(I.scalarMultiply(eps))

      val cov =
        regularizePD(Array2DRowRealMatrix(C.map(_.unsafeArray).unsafeArray)).
          scalarMultiply(Math.pow(sigma, 2.0))

      val distribution =
        MultivariateNormalDistribution(
          apacheRandom(rng),
          Array.fill(dim)(0.0),
          cov.getData
        )


      val s = distribution.sample()
      if s.exists(_.isNaN) then throw RuntimeException("Sample contains NaN")
      IArray.unsafeFromArray(s)

    .getOrElse:
      println(sigma)
      val cov = MatrixUtils.createRealIdentityMatrix(dim).scalarMultiply(Math.pow(sigma, 2.0))

      val distribution =
        MultivariateNormalDistribution(
          apacheRandom(rng),
          Array.fill(dim)(0.0),
          cov.getData
        )

      IArray.unsafeFromArray(distribution.sample())


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
          lhs.add(rhs).getData.map(IArray.unsafeFromArray)
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
          lhs.add(rhs).getData.map(IArray.unsafeFromArray)
        )

      a.copy(pc = newpc, C = newC)
