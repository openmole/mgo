/*
 * Copyright (C) 01/07/2020 Juste Raimbault
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
package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.tools.execution._
import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils, RealMatrix }
import org.apache.commons.math3.util.{ ArithmeticUtils, CombinatoricsUtils }

import monocle._
import monocle.syntax.all._

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.util.Random

/**
 * NSGA-III algorithm for many-objective problems
 *
 *  Deb, K., & Jain, H. (2013). An evolutionary many-objective optimization algorithm using reference-point-based nondominated sorting approach, part I: solving problems with box constraints. IEEE transactions on evolutionary computation, 18(4), 577-601.
 *
 * For U-NSGA-III, see
 *  Seada, H., & Deb, K. (2015, March). U-NSGA-III: a unified evolutionary optimization procedure for single, multiple, and many objectives: proof-of-principle results. In International conference on evolutionary multi-criterion optimization (pp. 34-49). Springer, Cham.
 *
 */
object NSGA3 {

  import CDGenome._
  import DeterministicIndividual._

  type NSGA3State = EvolutionState[Unit]

  def initialGenomes(populationSize: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(populationSize, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P](operatorExploration: Double, discrete: Vector[D], fitness: P => Vector[Double], reject: Option[Genome => Boolean], lambda: Int = -1): Breeding[S, Individual[P], Genome] =
    NSGA3Operations.adaptiveBreeding[S, Individual[P], Genome](
      individualFitness[P](fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      reject,
      operatorExploration,
      lambda)

  def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(express, components)

  def elitism[S, P](mu: Int, references: NSGA3Operations.ReferencePoints, components: Vector[C], fitness: P => Vector[Double]): Elitism[S, Individual[P]] =
    NSGA3Operations.elitism[S, Individual[P]](
      individualFitness[P](fitness),
      i => values(i.focus(_.genome).get, components),
      references,
      mu)

  case class Result[P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], individual: Individual[P])

  def result[P](population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[P]] = {
    val individuals = if (keepAll) population else keepFirstFront(population, individualFitness(fitness))
    individuals.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), i.focus(_.genome) andThen discreteValues get, individualFitness(fitness)(i), i)
    }
  }

  def result(nsga3: NSGA3, population: Vector[Individual[Vector[Double]]]): Vector[Result[Vector[Double]]] = result[Vector[Double]](population, nsga3.continuous, identity[Vector[Double]] _, keepAll = false)

  def reject(f: Option[(Vector[Double], Vector[Int]) => Boolean], continuous: Vector[C]): Option[Genome => Boolean] =
    f.map { reject => (g: Genome) =>
      val scaledContinuous = scaleContinuousValues(continuousValues.get(g), continuous)
      val discreteValue = discreteValues get g
      reject(scaledContinuous, discreteValue)
    }

  def reject(nsga3: NSGA3): Option[Genome => Boolean] = reject(nsga3.reject, nsga3.continuous)

  implicit def isAlgorithm: Algorithm[NSGA3, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    new Algorithm[NSGA3, Individual[Vector[Double]], Genome, NSGA3State] {
      override def initialState(t: NSGA3, rng: scala.util.Random): NSGA3State = EvolutionState(s = ())
      override def initialPopulation(t: NSGA3, rng: scala.util.Random): Vector[Individual[Vector[Double]]] =
        deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
          NSGA3.initialGenomes(t.popSize, t.continuous, t.discrete, reject(t), rng),
          NSGA3.expression(t.fitness, t.continuous))
      override def step(t: NSGA3): (NSGA3State, Vector[Individual[Vector[Double]]], Random) => (NSGA3State, Vector[Individual[Vector[Double]]]) =
        (s, population, rng) =>
          deterministic.step[NSGA3State, Individual[Vector[Double]], Genome](
            NSGA3.adaptiveBreeding[NSGA3State, Vector[Double]](t.operatorExploration, t.discrete, identity, reject(t)),
            NSGA3.expression(t.fitness, t.continuous),
            NSGA3.elitism[NSGA3State, Vector[Double]](t.popSize, t.referencePoints, t.continuous, identity),
            Focus[EvolutionState[Unit]](_.generation),
            Focus[EvolutionState[Unit]](_.evaluated))(s, population, rng)
    }

}

case class NSGA3(
  popSize: Int,
  referencePoints: NSGA3Operations.ReferencePoints,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  continuous: Vector[C],
  discrete: Vector[D] = Vector.empty,
  operatorExploration: Double = 0.1,
  reject: Option[(Vector[Double], Vector[Int]) => Boolean] = None)

object NSGA3Operations {

  def numberOfReferencePoints(divisions: Int, dimension: Int): Int = CombinatoricsUtils.binomialCoefficient(dimension + divisions - 1, divisions).toInt

  /**
   * reference points may either automatically computed given a fixed number, or provided by the user
   *  -> computation if needed done once and for all at initialization
   */
  case class ReferencePoints(references: Vector[Vector[Double]], normalized: Boolean = false)

  object ReferencePoints {
    def apply(divisions: Int, dimension: Int): ReferencePoints = ReferencePoints(simplexRefPoints(divisions, dimension))
  }

  /**
   * Field of fractions
   *  -> used for tricking comparison of vectors and have exact discrete points
   * @param n numerator
   * @param d denominator
   * @param reduced reduced with gcd?
   */
  case class Fraction(n: Int, d: Int, reduced: Boolean) {
    def +(f: Fraction): Fraction = Fraction(n * f.d + d * f.n, d * f.d)
    def -(f: Fraction): Fraction = Fraction(n * f.d - d * f.n, d * f.d)
    def *(f: Fraction): Fraction = Fraction(n * f.n, d * f.d)
    def /(f: Fraction): Fraction = Fraction(n * f.d, d * f.n)
    def *(p: Point): Point = Point(p.point.map { _ * this })
    def toDouble: Double = n.toDouble / d.toDouble
    def isPositive: Boolean = (n >= 0 && d >= 0) || (n <= 0 && d <= 0)
  }
  object Fraction {
    val zero: Fraction = Fraction(0, 1)
    val one: Fraction = Fraction(1, 1)
    def apply(x: Int): Fraction = Fraction(x, 1)
    def apply(n: Int, d: Int): Fraction = {
      // sign always at numerator the way fractions are constructed ?
      val gcd = ArithmeticUtils.gcd(n, d)
      Fraction(n / gcd, d / gcd, reduced = true)
    }
  }

  /**
   * Fractional points
   * @param point coordinates
   */
  case class Point(point: Vector[Fraction]) {
    def +(p: Point): Point = Point(point.zip(p.point).map { case (f1, f2) => f1 + f2 })
    def -(p: Point): Point = Point(point.zip(p.point).map { case (f1, f2) => f1 - f2 })
    def toDoubleVector: Vector[Double] = point.map(_.toDouble)
    def embedded(i: Int): Point = Point((0 until i by 1).map { i => point(i) }.toVector ++ Vector(Fraction.zero) ++ (i until point.size by 1).map { i => point(i) }.toVector)
    //def isPositive: Boolean = point.map{case Fraction(n,_,_) => n>0}.reduce(_&_)
    def isPositive: Boolean = point.map { _.isPositive }.reduce(_ && _)
    def isOnSimplex: Boolean = (point.reduce(_ + _) == Fraction.one) && isPositive // not the full simplex
  }

  /**
   * unit simplex points
   *  q: include generators? (n-1 generator vectors of dim n)
   * @param dimension dimension
   * @param divisions divisions on each dimension
   * @param points discrete point in the simplex
   */
  case class DiscreteUnitSimplex(
    dimension: Int,
    divisions: Int,
    points: Vector[Point]) {
    def embeddedPoints(i: Int): Vector[Point] = points.map(_.embedded(i))
  }

  object DiscreteUnitSimplex {

    /**
     * basic case : two dimensional simplex
     * @param divisions divisions
     * @return
     */
    def twoDimSimplex(divisions: Int): DiscreteUnitSimplex = {
      val coords = (0 to divisions by 1).map { Fraction(_, divisions) }
      val points = coords.zip(coords.reverse).map { case (f1, f2) => Point(Vector(f1, f2)) }.toVector
      DiscreteUnitSimplex(2, divisions, points)
    }

    /**
     * recursive constructor
     * two complementary hypersimplices are sufficient to generate the simplex in the next dimension
     * (brutal algorithm by filtering points - still a polynomial upper bound)
     *
     * @param dimension dimension
     * @param divisions number of divisions
     * @return
     */
    def apply(dimension: Int, divisions: Int): DiscreteUnitSimplex = {
      dimension match {
        case 1 => DiscreteUnitSimplex(1, divisions, (0 to divisions by 1).map { k => Point(Vector(Fraction(k, divisions))) }.toVector)
        case 2 => twoDimSimplex(divisions)
        case _ =>
          val prevSimplex = DiscreteUnitSimplex(dimension - 1, divisions)
          val emb0 = prevSimplex.embeddedPoints(0)
          val emb1 = prevSimplex.embeddedPoints(1)
          val origin = emb0(0)
          val points = (for {
            vi <- emb0.map(_ - origin)
            vj <- emb1.map(_ - origin)
          } yield origin + vi + vj).filter(_.isOnSimplex).distinct
          DiscreteUnitSimplex(dimension, divisions, points)
      }
    }

  }

  /**
   * Compute automatic reference points on the simplex
   *   (called at initialization)
   * @param divisions number of segments on each simplex bord line
   * @param dimension dimension of the space
   * @return
   */
  def simplexRefPoints(divisions: Int, dimension: Int): Vector[Vector[Double]] =
    DiscreteUnitSimplex(dimension, divisions).points.map { _.toDoubleVector }

  /**
   * NSGA3 breeding: next provisory population is of size 2*mu
   * filtering is done in elitism using reference points
   *
   * @param fitness fitness
   * @param genome genome
   * @param continuousValues continuous genome values function
   * @param continuousOperator continuousOperator
   * @param discreteValues discrete values
   * @param discreteOperator discreteOperator
   * @param discrete discrete
   * @param buildGenome buildGenome
   * @param tournamentRounds tournamentRounds
   * @param reject reject
   * @param operatorExploration operatorExploration
   * @param lambda breeded population size - in NSGA3, set at 2*population size - by default when lambda = -1
   * @tparam S state
   * @tparam I individual
   * @tparam G genome
   * @return
   */
  def adaptiveBreeding[S, I, G](
    fitness: I => Vector[Double],
    genome: I => G,
    continuousValues: G => Vector[Double],
    continuousOperator: G => Option[Int],
    discreteValues: G => Vector[Int],
    discreteOperator: G => Option[Int],
    discrete: Vector[D],
    buildGenome: (Vector[Double], Option[Int], Vector[Int], Option[Int]) => G,
    reject: Option[G => Boolean],
    operatorExploration: Double,
    lambda: Int = -1): Breeding[S, I, G] = (s, population, rng) => {

    val continuousOperatorStatistics = operatorProportions(genome andThen continuousOperator, population)
    val discreteOperatorStatistics = operatorProportions(genome andThen discreteOperator, population)

    def breedTwo: Breeding[S, I, G] = applyDynamicOperators[S, I, G](
      randomSelection[S, I],
      genome andThen continuousValues,
      genome andThen discreteValues,
      continuousOperatorStatistics,
      discreteOperatorStatistics,
      discrete,
      operatorExploration,
      buildGenome)

    val breededsize = if (lambda == -1) 2 * population.size else lambda

    breed(breedTwo, breededsize, reject)(s, population, rng)
  }

  /**
   * The particularity of nsga3 is at the elistism step
   *  - keep successive pareto fronts until having a population larger than the pop expected
   *  - remove the last front added
   *  - fill the remaining points with the reference points heuristic
   *
   *  Note: through normalization, ref points must be recomputed each time, even with user-defined points
   *   (option : number of points, taken within the objective simplex (tricky to compute ?) ; or user-defined)
   *
   * @param fitness fitness
   * @param values values
   * @param references reference points
   * @param mu population size
   * @tparam S state
   * @tparam I individual
   * @return
   */
  def elitism[S, I](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    references: ReferencePoints,
    mu: Int): Elitism[S, I] =
    (s, population, candidates, rng) =>
      (s, eliteWithReference[S, I](filterNaN(keepFirst(values)(population, candidates), fitness), fitness, references, mu)(rng))

  /**
   * Exact successive fronts computation
   *
   * @param population population
   * @param fitness fitness
   * @tparam I individual
   * @return Vector of fronts, coded by (individuals: Vector[I],fitnesses in same order: Vector(Vector(Double)),indices in initial population: Vector[Int])
   */
  def successiveFronts[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I], Vector[Vector[Double]], Vector[Int])] = {
    if (population.isEmpty) return Vector.empty[(Vector[I], Vector[Vector[Double]], Vector[Int])]

    // evaluate all fitness and put in map so that function are not reevaluated at each front computation
    val fitnesses = population.map(i => fitness(i))
    val fitnessmap = population.zip(fitnesses).toMap
    def compfitness: I => Vector[Double] = i => fitnessmap(i)

    def extractNextFront(state: (Vector[I], Vector[(I, Int)], Int)): (Vector[I], Vector[(I, Int)], Int) = {
      val (currentPop, frontnums, currentFrontNum) = state
      val currentFront = keepFirstFront(currentPop, compfitness)

      (currentPop.filter(i => !currentFront.contains(i)), frontnums ++ currentFront.map(i => (i, currentFrontNum)), currentFrontNum + 1)
    }
    val frontnums = Iterator.iterate[(Vector[I], Vector[(I, Int)], Int)]((population, Vector.empty, 0))(extractNextFront).
      takeWhile(_._1.nonEmpty).toSeq.last._2

    frontnums.
      toMap.
      zip(fitnesses).
      zipWithIndex.
      groupBy { case (((_, d), _), _) => d }.
      toVector.
      sortBy { _._1 }.
      map {
        case (_, v) =>
          (v.map { _._1._1._1 }.toVector, v.map { _._1._2 }.toVector, v.unzip._2.toVector)
      }
  }

  /**
   * extract elite using ref point heuristic
   * @param population population
   * @param fitness fitness
   * @param references reference
   * @param mu population size [size of elite is by default pop size / 2 (doubling population in breeding)]
   * @tparam S state
   * @tparam I individual
   * @return
   */
  def eliteWithReference[S, I](
    population: Vector[I],
    fitness: I => Vector[Double],
    references: ReferencePoints,
    mu: Int)(implicit rng: util.Random): Vector[I] = {
    if (population.size <= 1) return population
    // all successive Pareto fronts
    val allfronts: Vector[(Vector[I], Vector[Vector[Double]], Vector[Int])] = successiveFronts(population, fitness)
    //println(allfronts.size)
    if (allfronts.size == 0) return population

    val fronts = allfronts.map { _._1 }
    val fitnesses: Vector[Vector[Vector[Double]]] = allfronts.map { _._2 }
    val frontindices = allfronts.map { _._3 }
    val allfitnesses: Vector[Vector[Double]] = fitnesses.reduce { _ ++ _ }

    // check dimensions here (useful in NoisyNSGA3 if a foolish aggregation function has been provided)
    assert(
      allfitnesses.map(_.size).sum / allfitnesses.length == references.references.map(_.size).sum / references.references.length,
      "Incompatible dimension between objectives and reference points")

    val targetSize = mu

    // returns everything if not enough population (rq : this shouldnt happen)
    if (fronts.map { _.size }.sum < targetSize) fronts.flatten
    else {
      // else successive fronts
      val res = new ArrayBuffer[I]
      val cumsizes = new ArrayBuffer[Int]; cumsizes.append(0)
      val cumpops = new ArrayBuffer[Vector[I]]; cumpops.append(Vector.empty) // better to cache sucessive pops
      fronts.foreach { i =>
        if (res.size < targetSize) res.appendAll(i)
        cumsizes.append(cumsizes.last + i.size)
        cumpops.append(cumpops.last ++ i)
      }

      // return everything if good number
      if (res.size == targetSize) {
        res.toVector
      } else {
        // needs last front to be added and remove it ; ! remove the first element of cumsizes
        val lastfrontindex = cumsizes.tail.zipWithIndex.find { case (d, _) => d > targetSize }.get._2

        // indices of individuals in the last front
        val lastfrontinds = frontindices(lastfrontindex)

        val provpop: Vector[I] = if (lastfrontindex > 0) cumpops.tail(lastfrontindex - 1) else Vector.empty

        // next candidate points to be drawn in lastfront, given ref points -> normalize here
        val (normfitnesses, normreferences) = normalize(allfitnesses, references)

        def filter[T](v: Vector[T], indices: Vector[Int]): Vector[T] = v.zipWithIndex.filter { case (_, i) => indices.contains(i) }.map { _._1 }

        // niching in association to reference points ; selection according to it - requires last front indices
        val additionalPoints = referenceNichingSelection[S, I](
          filter[Vector[Double]](normfitnesses, lastfrontinds),
          normreferences,
          filter[I](population, lastfrontinds),
          targetSize - provpop.size)

        provpop ++ additionalPoints
      }
    }

  }

  /**
   * normalize objectives and compute normalized reference points
   *   - for each dimension :
   *      * compute ideal point
   *      * translate objectives to have min at 0
   *      * compute extreme points
   *   - construct simplex and compute intercepts a_j
   *   - for each dimension, normalize translated objective
   *
   * @param fitnesses vector of fitnesses
   * @param references reference points
   * @return (normalized fitnesses ; normalized reference points)
   */
  def normalize(fitnesses: Vector[Vector[Double]], references: ReferencePoints): (Vector[Vector[Double]], Vector[Vector[Double]]) = {
    // ideal point, translation and extreme points
    val (translated, maxpoints) = translateAndMaxPoints(fitnesses)
    //println("max points = " + maxpoints)
    val intercepts = simplexIntercepts(maxpoints)
    //println("intercepts = " + intercepts)
    (normalizeMax(translated, intercepts), computeReferencePoints(references, intercepts))
  }

  /**
   * Translate to have ideal point at \vec{0} ; compute max points
   *  ! in case of a common max point for different dimensions, the intercepts can not be computed
   *   -> we remove the common point and recompute the max points
   *
   * @param fitnesses fitnesses
   * @return (translated fitnesses , indices of max point for each dimension)
   */
  def translateAndMaxPoints(fitnesses: Vector[Vector[Double]]): (Vector[Vector[Double]], Vector[Vector[Double]]) = {
    val d = fitnesses(0).length
    //println(fitnesses.map(_.size))
    val idealValues = fitnesses.transpose.map { _.min }
    val translated = fitnesses.map { _.zip(idealValues).map { case (f, mi) => f - mi } }
    //assert(translated.flatten.min >= 0.0, "negative translated data")

    // max points minimize the Achievement Scalarizing Function
    val weights: Vector[Vector[Double]] = Vector.tabulate(d, d) { case (i, j) => if (i == j) 1.0.toDouble else 1e-6.toDouble }
    def maxPoints(values: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      val maxinds = weights.map { ei =>
        values.map {
          xi =>
            xi.zip(ei).map {
              case (xij, eij) => xij * eij
            }.max
        }.zipWithIndex.maxBy { case (dd, _) => dd }._2
      }
      if (maxinds.toSet.size < maxinds.size) {
        //println("spurious double max! - removing one point")
        val ginds: Seq[(Int, Vector[Int])] = maxinds.groupBy(i => i).toSeq
        val removedind = ginds(ginds.map(_._2.size).indexWhere(_ > 1))._1
        maxPoints(values.zipWithIndex.filter(_._2 != removedind).map(_._1))
      } else maxinds.map(values(_))
    }

    (translated, maxPoints(translated))
  }

  /**
   * Compute the intercepts on each dimension axis of the simplex generated by the N points given
   * @param maxPoints (MUST have N points to have an hyperplan)
   * @return
   */
  def simplexIntercepts(maxPoints: Vector[Vector[Double]]): Vector[Double] = {
    // ensure that no dimension is flat - otherwise the intercept is infinite
    // arbitrarily x2 point with min norm other dimension, /2 with max (then strictly not max point - but flat objective should not be used
    // and quickly disappears for the embedding dimension in NoisyEA after first gen
    // note that this will not work if all dimensions are flat
    val dimflatness = maxPoints.transpose.map(_.max).zip(maxPoints.transpose.map(_.min)).map { case (ma, mi) => ma - mi }
    val modifinds: Vector[Option[(Int, Int)]] = dimflatness.zipWithIndex.map {
      case (delta, d) => if (delta != 0.0) None else {
        val norms = maxPoints.map(_.zipWithIndex.map { case (x, dd) => if (dd == d) 0.0 else x * x }.sum)
        val (mi, ma) = (norms.min, norms.max)
        Some((norms.indexOf(mi), norms.indexOf(ma)))
      }
    }

    val correctedPoints = maxPoints.zipWithIndex.map {
      case (p, i) =>
        p.zip(modifinds).map {
          case (x, None) => x
          case (x, Some((imin, _))) if imin == i => 2 * x
          case (x, Some((_, imax))) if imax == i => x / 2
          case (x, _) => x
        }
    }

    val lastPoint = correctedPoints(correctedPoints.length - 1)
    val dim = lastPoint.size

    val translated: Vector[Vector[Double]] = correctedPoints.map { _.zip(lastPoint).map { case (xij, x1j) => xij - x1j } }

    // compute cross-product
    val coefs = (0 until dim).map { i =>
      new LUDecomposition(
        MatrixUtils.createRealMatrix(
          translated.dropRight(1).map(_.toArray).toArray ++ Array(Array.tabulate(dim)(j => if (j == i) 1.0.toDouble else 0.0.toDouble)))).getDeterminant
    }

    // hyperplan equation is then coefs \cdot (x - x0) = 0 -> intercepts at xj=0 for j != i
    val intercepts = (0 until dim).map { i =>
      lastPoint(i) + coefs.zip(lastPoint).zipWithIndex.filter(c => c._2 != i).map { case ((c, x), _) => c * x / coefs(i) }.sum
    }.toVector
    assert(!intercepts.exists(_.isNaN), "Simplex intercepts have NaN")
    intercepts
  }

  /**
   * normalize to have max at 1
   * @param points points
   * @param maxvals max values for each dimension
   * @return
   */
  def normalizeMax(points: Vector[Vector[Double]], maxvals: Vector[Double]): Vector[Vector[Double]] =
    points.transpose.zip(maxvals).map { case (p, m) => p.map { _ / m } }.transpose

  /**
   * normalize ref points if needed (when non normalized reference points provided by the user are used)
   * @param references references
   * @param intercepts intercepts
   * @return
   */
  def computeReferencePoints(references: ReferencePoints, intercepts: Vector[Double]): Vector[Vector[Double]] = references match {
    case ReferencePoints(r, false) => normalizeMax(r, intercepts)
    case ReferencePoints(r, true) => r
  }

  /**
   * Aggregate normalized fitnesses on reference points ; select on this.
   * @param normalizedFitnesses normalized fitness values
   * @param normalizedReferences normalized reference points
   * @param pointsNumber number of points to select
   * @return indices of selected individuals
   *          (population not needed at this stage)
   */
  def referenceNichingSelection[S, I](
    normalizedFitnesses: Vector[Vector[Double]],
    normalizedReferences: Vector[Vector[Double]],
    population: Vector[I],
    pointsNumber: Int)(implicit rng: util.Random): Vector[I] = {

    //println("Adding " + pointsNumber + " points among " + population.size)
    //println("Normalized fitnesses length = " + normalizedFitnesses.length)

    //val normFitnessMap = population.zip(normalizedFitnesses).toMap
    val assocMap = associateReferencePoints(normalizedFitnesses, normalizedReferences, population) // associate points to references
    //println("association of ref points = "+assocMap)
    val (finalAssocMap, selected) = pointsSelection(assocMap, Vector.empty, pointsNumber)
    //println("distinct niched ref points = " + selected.map { _._2 }.distinct)
    //println("rel min x sel points = " + selected.map(s => normFitnessMap(s._1)(0)).min)
    //println("rel min y sel points = " + selected.map(s => normFitnessMap(s._1)(1)).min)
    selected.map { _._1 }
  }

  /**
   * Compute reference lines, distances, and associate points to references
   *  - unoptimized, shouldnt recreate the matrices at each run
   * @param points points
   * @param references references
   * @param population population
   * @return map point i => ref point j,distance
   */
  def associateReferencePoints[I](
    points: Vector[Vector[Double]],
    references: Vector[Vector[Double]],
    population: Vector[I]): Map[I, (Int, Double)] = {

    assert(references.map(_.filter(_.isNaN).isEmpty).reduce(_ && _), "Ref points have NaN")

    val refnormsquared = references.map { _.map { x => x * x }.sum }
    //println(refnormsquared)

    // projection of x on dim is (\vec{u}\cdot \vec{x} \vec{u}) with \vec{u} = \vec{r}_dim / ||\vec{r}_dim||
    def proj(dim: Int, x: Vector[Double]): Vector[Double] = {
      val w = MatrixUtils.createColumnRealMatrix(references(dim).toArray)
      w.multiply(MatrixUtils.createRowRealMatrix(x.toArray)).multiply(w).getColumn(0).map { _ / refnormsquared(dim) }.toVector
    }

    points.zip(population).map {
      case (point, individual) =>

        // for each reference point, compute distance using projection
        val dists = references.indices.map {
          i =>
            val projected = proj(i, point)
            //println(projected)
            math.sqrt(point.zip(projected).map { case (x, y) => (x - y) * (x - y) }.sum)
        }
        val mindist = dists.min
        //println(dists)
        (individual, (dists.zipWithIndex.filter { case (d, _) => d == mindist }.map { case (_, j) => j }.head, mindist))
    }.toMap
  }

  /**
   * Select points given the association to closest reference point
   * @param associationMap association map: individual => (index of ref point, distance)
   * @param selected points already selected
   * @param toselect number of points to select
   * @param rng rng
   * @tparam I individual
   * @return (association map, selected individuals)
   */
  def pointsSelection[I](
    associationMap: Map[I, (Int, Double)],
    selected: Vector[(I, Int)],
    toselect: Int)(implicit rng: util.Random): (Map[I, (Int, Double)], Vector[(I, Int)]) = {
    toselect match {
      case n if n == 0 => (associationMap, selected)
      case _ =>
        val selectedRefCount = selected.groupBy(_._2).toSeq.map { g => (g._1, g._2.size) }.toMap
        val refCount = associationMap.toSeq.map { _._2._1 }.toVector.distinct.map { j => (j, selectedRefCount.getOrElse(j, 0)) }.toMap
        val (jmin, _) = refCount.toVector.minBy(_._2) // index of ref point with minimal number of associated points
        val candidatePoints = associationMap.filter { case (_, (j, _)) => j == jmin } // cannot be 0 the way it is constructed
        val newpoint = if (refCount(jmin) == 0) candidatePoints.toVector.minBy { _._2._2 }._1 else {
          candidatePoints.toVector(rng.nextInt(candidatePoints.toVector.size))._1
        }
        pointsSelection(associationMap.filter { _._1 != newpoint }, selected ++ Vector((newpoint, jmin)), toselect - 1)
    }
  }

}

