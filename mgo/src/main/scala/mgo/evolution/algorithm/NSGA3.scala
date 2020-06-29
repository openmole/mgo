package mgo.evolution.algorithm

import cats.implicits._
import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.tools.execution._
import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils, RealMatrix }
import org.apache.commons.math3.util.ArithmeticUtils

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

  def initialGenomes(populationSize: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random) =
    CDGenome.initialGenomes(populationSize, continuous, discrete, reject, rng)

  def adaptiveBreeding[S, P](operatorExploration: Double, discrete: Vector[D], fitness: P => Vector[Double], reject: Option[Genome => Boolean]): Breeding[S, Individual[P], Genome] =
    NSGA3Operations.adaptiveBreeding[S, Individual[P], Genome](
      individualFitness[P](fitness),
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      _ => 1,
      reject,
      operatorExploration)

  def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(express, components)

  def elitism[S, P](mu: Int, references: NSGA3Operations.ReferencePoints, components: Vector[C], fitness: P => Vector[Double]): Elitism[S, Individual[P]] =
    NSGA3Operations.elitism[S, Individual[P]](
      individualFitness[P](fitness),
      i => values(Individual.genome[P].get(i), components),
      references,
      mu)

  case class Result(continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double])

  def result[P](population: Vector[Individual[P]], continuous: Vector[C], fitness: P => Vector[Double]): Vector[Result] =
    population.map { i =>
      Result(scaleContinuousValues(continuousValues.get(i.genome), continuous), Individual.genome composeLens discreteValues get i, individualFitness(fitness)(i))
    }

  def result(nsga3: NSGA3, population: Vector[Individual[Vector[Double]]]): Vector[Result] = result[Vector[Double]](population, nsga3.continuous, identity[Vector[Double]] _)

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
            EvolutionState.generation)(s, population, rng)
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
    val zero = Fraction(0, 1)
    val one = Fraction(1, 1)
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
    tournamentRounds: Int => Int,
    reject: Option[G => Boolean],
    operatorExploration: Double): Breeding[S, I, G] = (s, population, rng) => {

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

    breed(breedTwo, 2 * population.size, reject)(s, population, rng)
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
    //println("elite with ref - pop size "+population.size)
    val allfronts = successiveFronts(population, fitness)
    //println("number of pareto fronts = "+allfronts.size)
    val fronts = allfronts.map { _._1 }
    //println("front sizes = "+fronts.map{_.size})
    val fitnesses = allfronts.map { _._2 }
    val frontindices = allfronts.map { _._3 }
    val allfitnesses = fitnesses.reduce { _ ++ _ }

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
      if (res.size == targetSize) res.toVector
      else {
        // tricky part
        // needs last front to be added and remove it ; ! remove the first element of cumsizes
        val lastfrontindex = cumsizes.tail.zipWithIndex.find { case (d, _) => d > targetSize }.get._2
        //println("last front index = "+lastfrontindex+" / frontindices size = "+frontindices.size)
        //val lastfront = cumpops.tail(lastfrontindex)
        // indices of individuals in the last front
        val lastfrontinds = frontindices(lastfrontindex)
        println("last front indices = " + lastfrontinds)

        val provpop: Vector[I] = if (lastfrontindex > 0) cumpops.tail(lastfrontindex - 1) else Vector.empty
        //println("previous pop size = "+provpop.size)

        // next candidate points to be drawn in lastfront, given ref points
        // -> normalize here
        val (normfitnesses, normreferences) = normalize(allfitnesses, references)

        def filter[T](v: Vector[T], indices: Vector[Int]): Vector[T] = v.zipWithIndex.filter { case (_, i) => indices.contains(i) }.map { _._1 }

        // niching in association to reference points ; selection according to it
        // needs last front indices
        //val additionalPointsIndices = referenceNichingSelection[M](normfitnesses,normreferences,lastfrontinds,targetSize - provpop.size)//(rng=rng)
        //val additionalPoints = population.zipWithIndex.filter{case (_,i) => additionalPointsIndices.contains(i)}.map{case (ind,_) => ind}
        val additionalPoints = referenceNichingSelection[S, I](
          filter[Vector[Double]](normfitnesses, lastfrontinds),
          //filter[Vector[Double]](normreferences, lastfrontinds),
          normreferences,
          filter[I](population, lastfrontinds),
          targetSize - provpop.size)
        //println("size of final elite population : "+provpop.size+" + "+additionalPoints.size)

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
    println("max points = " + maxpoints)
    val intercepts = simplexIntercepts(maxpoints)
    println("intercepts = " + intercepts)
    (normalizeMax(translated, intercepts), computeReferencePoints(references, intercepts))
  }

  /**
   * Translate to have ideal point at \vec{0} ; compute max points
   *
   * @param fitnesses fitnesses
   * @return (translated fitnesses , indices of max point for each dimension)
   */
  def translateAndMaxPoints(fitnesses: Vector[Vector[Double]]): (Vector[Vector[Double]], Vector[Vector[Double]]) = {
    val d = fitnesses(0).length
    val idealValues = fitnesses.transpose.map { _.min }
    val translated = fitnesses.map { _.zip(idealValues).map { case (f, mi) => f - mi } }
    //assert(translated.flatten.min >= 0.0, "negative translated data")
    // max points minimize the Achievement Scalarizing Function
    val maxIndices: Vector[Int] = Vector.tabulate(d, d) { case (i, j) => if (i == j) 1.0 else 1e-6 }.map { ei: Vector[Double] => translated.map { xi => xi.zip(ei).map { case (xij, eij) => xij * eij }.max }.zipWithIndex.maxBy { case (d, _) => d }._2 }
    //println(maxIndices)
    (translated, maxIndices.map(fitnesses(_)))
  }

  /**
   * Compute the intercepts on each dimension axis of the simplex generated by the N points given
   * @param maxPoints (MUST have N points to have an hyperplan)
   * @return
   */
  def simplexIntercepts(maxPoints: Vector[Vector[Double]]): Vector[Double] = {
    val firstPoint = maxPoints(0)
    val dim = firstPoint.size

    val translated: Vector[Vector[Double]] = maxPoints.map { _.zip(firstPoint).map { case (xij, x1j) => xij - x1j } }
    val baseChange: RealMatrix = MatrixUtils.createRealMatrix((Vector(firstPoint.map { xj => -xj }) ++ translated.tail).map { _.toArray }.toArray)

    // check that the new basis is not singular
    assert(new LUDecomposition(baseChange).getDeterminant != 0, "singular matrix : " + baseChange.toString + "\n max points are : " + maxPoints)

    def getDiag(m: RealMatrix): Vector[Double] = m.getData.zipWithIndex.map { case (row, i) => row(i) }.toVector
    getDiag(
      MatrixUtils.inverse(baseChange). //multiply(MatrixUtils.createRealDiagonalMatrix(Array.fill(dim)(1.0))). // apply to basis vectors ~ multiply by identity
        add(MatrixUtils.createRealMatrix(Array.fill(dim)(firstPoint.toArray)).transpose()))
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
    //println("Ref points = "+normalizedReferences)
    println("Adding " + pointsNumber + " points among " + population.size)
    println("Normalized fitnesses length = " + normalizedFitnesses.length)
    println("references length = " + normalizedReferences.length)
    // FIXME normalized fitnesses are wrong

    //val normFitnessMap = population.zip(normalizedFitnesses).toMap
    val assocMap = associateReferencePoints(normalizedFitnesses, normalizedReferences, population) // associate points to references
    //println("association of ref points = "+assocMap)
    val (finalAssocMap, selected) = pointsSelection(assocMap, Vector.empty, pointsNumber)
    println("distinct niched ref points = " + selected.map { _._2 }.distinct)
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
    val refnormsquared = references.map { _.map { x => x * x }.sum }

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
            math.sqrt(point.zip(proj(i, point)).map { case (x, y) => (x - y) * (x - y) }.sum)
        }
        val mindist = dists.min
        (individual, (dists.zipWithIndex.filter { case (d, _) => d == mindist }.map { case (_, j) => j }.head, mindist))
    }.toMap
  }

  /**
   * Select points given the association to closest reference point
   * @param associationMap association map
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
    //println("Selecting "+toselect+" points from "+associationMap.toVector.size)
    toselect match {
      case n if n == 0 => (associationMap, selected)
      case _ => {
        //val refCount = selected.groupBy(_._2).map{g => (g._1,g._2.size)}
        //val refCount = associationMap.toVector.groupBy(_._2._1).map{g => (g._1,g._2.size)} // ref with no count can not be in the refcount
        val selectedRefCount = selected.groupBy(_._2).toSeq.map { g: (Int, Vector[(I, Int)]) => (g._1, g._2.size) }.toMap
        val refCount = associationMap.toSeq.map { _._2._1 }.toVector.distinct.map { j => (j, selectedRefCount.getOrElse(j, 0)) }.toMap
        val (jmin, _) = refCount.toVector.minBy(_._2) // index of ref point with minimal number of associated points
        val candidatePoints = associationMap.filter { case (_, (j, _)) => j == jmin } // cannot be 0 the way it is constructed
        //val newpointIndex = if(refCount(jmin)==0) candidatePoints.minBy{_._2._2}._1 else  candidatePoints.minBy{_._2._2}._1
        //val newpointIndex = candidatePoints.minBy{_._2._2}._1 // taking the min dist point leads to an overcrowding of some points only
        val newpoint = if (refCount(jmin) == 0) candidatePoints.toVector.minBy { _._2._2 }._1 else {
          candidatePoints.toVector(rng.nextInt(candidatePoints.toVector.size))._1
        }
        pointsSelection(associationMap.filter { _._1 != newpoint }, selected ++ Vector((newpoint, jmin)), toselect - 1)
      }
    }
  }

}

