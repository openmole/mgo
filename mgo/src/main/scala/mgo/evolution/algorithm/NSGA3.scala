package mgo.evolution.algorithm

import cats.implicits._

import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.dominance._
import mgo.evolution.elitism._
import mgo.tools.execution._

import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils, RealMatrix }
import org.apache.commons.math3.util.ArithmeticUtils
import shapeless._

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.language.higherKinds

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
      override def initialState(t: NSGA3, rng: scala.util.Random) = EvolutionState(s = Unit)
      override def initialPopulation(t: NSGA3, rng: scala.util.Random) =
        deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
          NSGA3.initialGenomes(t.popSize, t.continuous, t.discrete, reject(t), rng),
          NSGA3.expression(t.fitness, t.continuous))
      override def step(t: NSGA3) =
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
   * @param n
   * @param d
   * @param reduced
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
      Fraction((n / gcd).toInt, (d / gcd).toInt, true)
    }
  }

  /**
   * Fractional points
   * @param point
   */
  case class Point(point: Vector[Fraction]) {
    def +(p: Point): Point = Point(point.zip(p.point).map { case (f1, f2) => f1 + f2 })
    def -(p: Point): Point = Point(point.zip(p.point).map { case (f1, f2) => f1 - f2 })
    def toDoubleVector: Vector[Double] = point.map(_.toDouble)
    def embedded(i: Int): Point = Point((0 to i - 1 by 1).map { i => point(i) }.toVector ++ Vector(Fraction.zero) ++ (i until point.size by 1).map { i => point(i) }.toVector)
    //def isPositive: Boolean = point.map{case Fraction(n,_,_) => n>0}.reduce(_&_)
    def isPositive: Boolean = point.map { _.isPositive }.reduce(_ && _)
    def isOnSimplex: Boolean = (point.reduce(_ + _) == Fraction.one) && isPositive // not the full simplex
  }

  case class DiscreteUnitSimplex(
    dimension: Int,
    divisions: Int,
    /**
     * discrete point in the simplex
     */
    points: Vector[Point] //,

  /**
   * n-1 generator vectors of dim n
   */
  //generators: Vector[Point]
  ) {
    def embeddedPoints(i: Int): Vector[Point] = points.map(_.embedded(i))
  }

  object DiscreteUnitSimplex {

    /**
     * basic case : two dimensional simplex
     * @param divisions
     * @return
     */
    def twoDimSimplex(divisions: Int): DiscreteUnitSimplex = {
      val coords = (0 to divisions by 1).map { Fraction(_, divisions) }
      val points = coords.zip(coords.reverse).map { case (f1, f2) => Point(Vector(f1, f2)) }.toVector
      //val generator = points(1) - points(0)
      //DiscreteUnitSimplex(2,divisions,points,Vector(generator))
      DiscreteUnitSimplex(2, divisions, points)
    }

    // def embeddedGenerators(simplex: DiscreteUnitSimplex,direction: Int): Vector[Point] = simplex.generators.map(_.embedded(direction))

    /**
     * recursive constructor
     * two complementary hypersimplices are anough to generate the simplex in the next dimension
     * (rough algorithm by filtering points - still a polynomial upper bound)
     *
     * @param dimension
     * @param divisions
     * @return
     */
    def apply(dimension: Int, divisions: Int): DiscreteUnitSimplex = {
      dimension match {
        case 2 => twoDimSimplex(divisions)
        case _ => {
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

  }

  /**
   * compute auto ref points on the simplex
   *   (called at initialization)
   * @param divisions number of segments on each simplex bord line
   * @param dimension dimension of the space
   * @return
   */
  def simplexRefPoints(divisions: Int, dimension: Int): Vector[Vector[Double]] = {
    //println("Computing simplex reference points with "+divisions+" divisions in dimension "+dimension)
    //val start = System.currentTimeMillis()
    val res = DiscreteUnitSimplex(dimension, divisions).points.map { _.toDoubleVector }
    //println("n = "+dimension+" ; p = "+divisions+" ; t = "+(System.currentTimeMillis() - start))
    res
  }

  /**
   * NSGA3 breeding : next provisory population is of size 2*mu
   *  ; filtering is done in elitism using reference points
   *
   * @param fitness
   * @param genome
   * @param genomeValues
   * @param buildGenome
   * @param crossover
   * @param mutation
   * @param lambda
   * @tparam M
   * @tparam I
   * @tparam G
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
   *  Note : through normalization, ref points must be recomputed each time, even with user-defined points
   *   (option : number of points, taken within the objective simplex (tricky to compute ?) ; or user-defined)
   *
   * @param fitness
   * @param values
   * @param mu
   * @tparam M
   * @tparam I
   * @return
   */
  def elitism[S, I](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    references: ReferencePoints,
    mu: Int): Elitism[S, I] =
    (s, population, candidates, rng) => (s, eliteWithReference[S, I](filterNaN(keepFirst(values)(population, candidates), fitness), fitness, references, mu)(rng))

  /**
   * Successive fronts by number of dominating points
   *  (grouping by number of dominating points gives successive fronts)
   *
   * FIXME: number of dominating DOES NOT exactly correspond to number of front ?
   *
   * @param population
   * @param fitness
   * @tparam I
   * @return fronts in order, with population and corresponding fitness values
   */
  def successiveFrontsApproxNumberofDominating[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I], Vector[Vector[Double]])] = {
    // fitness evaluation is done here in number of dominating
    val (fitnesses, dominating) = numberOfDominatingAndFitnesses(fitness, population)
    population.zip(dominating).zip(fitnesses).groupBy { case ((_, d), f) => d.value }.toVector.sortBy { _._1 }.map { case (_, v) => (v.map { _._1._1 }, v.map { _._2 }) }
  }

  /**
   * redefine dominating function as also needs fitness values
   * @param fitness
   * @param values
   * @param dominance
   * @tparam I
   * @return
   */
  def numberOfDominatingAndFitnesses[I](
    fitness: I => Vector[Double],
    values: Vector[I],
    dominance: Dominance = nonStrictDominance): (Vector[Vector[Double]], Vector[Lazy[Int]]) = {
    val fitnesses = values.map(i => fitness(i))

    def ranks =
      fitnesses.zipWithIndex.map {
        case (v1, index1) =>
          def containsNaN = v1.exists(_.isNaN)
          def otherIndividuals = fitnesses.zipWithIndex.filter { case (_, index2) => index1 != index2 }
          def numberOfDominatingIndividual = otherIndividuals.count { case (v2, _) => dominance.isDominated(v1, v2) }
          shapeless.Lazy(if (containsNaN) Int.MaxValue else numberOfDominatingIndividual)
      }
    (fitnesses, ranks)
  }

  /**
   * Exact successive fronts computation
   * @param population
   * @param fitness
   * @tparam I
   * @return Vector of fronts, coded by (individuals: Vector[I],
   *         fitnesses in same order: Vector[Vector[Double]],
   *         indices in initial population: Vector[Int])
   */
  def successiveFronts[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Vector[I], Vector[Vector[Double]], Vector[Int])] = {
    // evaluate all fitness and put in map so that function are not reevaluated at each front computation
    val fitnesses = population.map(i => fitness(i))

    val fitnessmap = population.zip(fitnesses).toMap
    def compfitness: I => Vector[Double] = i => fitnessmap(i)
    //val frontnums = new mutable.HashMap[I,Lazy[Int]]
    val frontnums = new HashMap[I, Int]
    var currentPop = population; var currentFrontNum = 0
    while (frontnums.keySet.size < population.size) {
      //println(currentFrontNum+" - "+frontnums.keySet.size+"/"+population.size)
      val currentFront = keepFirstFront(currentPop, compfitness)
      //println(ranking.numberOfDominating(compfitness, currentPop))
      currentPop = currentPop.filter(i => !currentFront.contains(i))
      currentFront.foreach(i => frontnums.put(i, currentFrontNum))
      currentFrontNum = currentFrontNum + 1
    }
    frontnums.toMap.zip(fitnesses).zipWithIndex.groupBy { case (((_, d), f), j) => d }.toVector.sortBy { _._1 }.map { case (_, v) => (v.map { _._1._1._1 }.toVector, v.map { _._1._2 }.toVector, v.values.toVector) }
  }

  /**
   * extract elite using ref point heuristic
   * @param population
   * @param fitness
   * @param reference
   * @tparam I
   * @return
   */
  def eliteWithReference[S, I](
    population: Vector[I],
    fitness: I => Vector[Double],
    references: ReferencePoints,
    // size of elite is by default pop size / 2 (doubling population in breeding)
    mu: Int)(implicit rng: util.Random): Vector[I] = {
    //println("elite with ref - pop size "+population.size)
    val allfronts = successiveFronts(population, fitness)
    //println("number of pareto fronts = "+allfronts.size)
    val fronts = allfronts.map { _._1 }
    //println("front sizes = "+fronts.map{_.size})
    val fitnesses = allfronts.map { _._2 }
    val frontindices = allfronts.map { _._3 }
    val allfitnesses = fitnesses.reduce { _ ++ _ } //{(v:(Vector[Vector[Double]],Vector[Vector[Double]])) => v._1++v._2} // dirty flatten

    //val targetSize = mu(population)
    val targetSize = mu

    // returns everything if not enough population (rq : this shouldnt happen)
    if (fronts.map { _.size }.sum < targetSize) fronts.flatten
    else {
      // else successive fronts
      val res = new ArrayBuffer[I]
      val cumsizes = new ArrayBuffer[Int]; cumsizes.append(0)
      val cumpops = new ArrayBuffer[Vector[I]]; cumpops.append(Vector.empty) // better to cache sucessive pops
      fronts.foreach { i =>
        if (res.size < targetSize) res.append(i: _*)
        cumsizes.append(cumsizes.last + i.size)
        cumpops.append(cumpops.last ++ i)
      }

      //println("cumulated front sizes : "+cumsizes)

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

        val provpop: Vector[I] = if (lastfrontindex > 0) cumpops.tail(lastfrontindex - 1) else Vector.empty
        //println("previous pop size = "+provpop.size)

        // next candidate points to be drawn in lastfront, given ref points
        // -> normalize here
        val (normfitnesses, normreferences) = normalize(allfitnesses, references)

        def filter[I](v: Vector[I], indices: Vector[Int]): Vector[I] = v.zipWithIndex.filter { case (_, i) => indices.contains(i) }.map { _._1 }

        // niching in association to reference points ; selection according to it
        // needs last front indices
        //val additionalPointsIndices = referenceNichingSelection[M](normfitnesses,normreferences,lastfrontinds,targetSize - provpop.size)//(rng=rng)
        //val additionalPoints = population.zipWithIndex.filter{case (_,i) => additionalPointsIndices.contains(i)}.map{case (ind,_) => ind}
        val additionalPoints = referenceNichingSelection[S, I](filter[Vector[Double]](normfitnesses, lastfrontinds), filter[Vector[Double]](normreferences, lastfrontinds), filter[I](population, lastfrontinds), targetSize - provpop.size)
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
   * @param fitnesses
   * @param references
   * @return (normalized fitnesses ; normalized reference points)
   */
  def normalize(fitnesses: Vector[Vector[Double]], references: ReferencePoints): (Vector[Vector[Double]], Vector[Vector[Double]]) = {
    // ideal point, translation and extreme points
    val (translated, maxpoints) = translateAndMaxPoints(fitnesses)
    val intercepts = simplexIntercepts(maxpoints)
    (normalizeMax(translated, intercepts), computeReferencePoints(references, intercepts))
  }

  /**
   * Translate to have ideal point at \vec{0} ; compute max points
   *
   * @param fitnesses
   * @return (translated fitnesses , max points indices for each dimension)
   */
  def translateAndMaxPoints(fitnesses: Vector[Vector[Double]]): (Vector[Vector[Double]], Vector[Vector[Double]]) = {
    // TODO check if transpose has expected behavior
    val idealValues = fitnesses.transpose.map { _.min }
    println("mins = " + idealValues)
    println("maxs = " + fitnesses.transpose.map { _.max })
    val translated = fitnesses.map { _.zip(idealValues).map { case (f, mi) => f - mi } }
    assert(translated.flatten.min >= 0.0, "negative translated data")
    (translated, translated.transpose.map { v => translated(v.zipWithIndex.minBy { case (d, _) => d }._2) })
  }

  /**
   * Compute the intercepts on each dimension axis of the simplex generated by the N points given
   * @param maxPoints (MUST have N points to have an hyperplan)
   * @return
   */
  def simplexIntercepts(maxPoints: Vector[Vector[Double]]): Vector[Double] = {
    val firstPoint = maxPoints(0)
    val dim = firstPoint.size

    // FIXME problem with translation here ?
    val translated: Vector[Vector[Double]] = maxPoints.map { _.zip(firstPoint).map { case (xij, x1j) => xij - x1j } }
    val baseChange: RealMatrix = MatrixUtils.createRealMatrix((Vector(firstPoint.map { case xj => -xj }) ++ translated.tail).map { _.toArray }.toArray)

    // check that the new basis is not singular
    assert((new LUDecomposition(baseChange)).getDeterminant != 0, "singular matrix : " + baseChange.toString + "\n max points are : " + maxPoints)

    def getDiag(m: RealMatrix): Vector[Double] = m.getData.zipWithIndex.map { case (row, i) => row(i) }.toVector
    getDiag(MatrixUtils.inverse(baseChange).multiply(MatrixUtils.createRealDiagonalMatrix(Array.fill(dim)(1.0))).add(MatrixUtils.createRealMatrix(Array.fill(dim)(firstPoint.toArray)).transpose()))
  }

  /**
   * normalize to have max at 1
   * @param points
   * @param maxvals
   * @return
   */
  def normalizeMax(points: Vector[Vector[Double]], maxvals: Vector[Double]): Vector[Vector[Double]] =
    points.transpose.zip(maxvals).map { case (p, m) => p.map { _ / m } }.transpose

  /**
   * normalize ref points if needed
   * @param references
   * @param intercepts
   * @return
   */
  def computeReferencePoints(references: ReferencePoints, intercepts: Vector[Double]): Vector[Vector[Double]] = references match {
    case ReferencePoints(r, false) => normalizeMax(r, intercepts)
    case ReferencePoints(r, true) => r
  }

  /**
   * Aggregate normalized fitnesses on reference points ; select on this.
   * @param normalizedFitnesses
   * @param normalizedReferences
   * @param selectionIndices indices of the set in which to select additional points
   * @param pointsNumber number of points to select
   * @return indices of selected individuals
   *          (population not needed at this stage)
   */
  def referenceNichingSelection[S, I](
    normalizedFitnesses: Vector[Vector[Double]],
    normalizedReferences: Vector[Vector[Double]],
    //selectionIndices: Vector[Int],
    population: Vector[I],
    pointsNumber: Int)(implicit rng: util.Random): Vector[I] = {
    //println("Ref points = "+normalizedReferences)
    println("Adding " + pointsNumber + " points among " + population.size)
    println(normalizedFitnesses)
    // FIXME normalized fitnesses are wrong

    val normFitnessMap = population.zip(normalizedFitnesses).toMap
    val assocMap = associateReferencePoints(normalizedFitnesses, normalizedReferences, population) // associate points to references
    //println("association of ref points = "+assocMap)
    val (finalAssocMap, selected) = pointsSelection(assocMap, Vector.empty, pointsNumber)(new util.Random)
    println("distinct niched ref points = " + selected.map { _._2 }.distinct)
    println("rel min x sel points = " + selected.map(s => normFitnessMap(s._1)(0)).min)
    println("rel min y sel points = " + selected.map(s => normFitnessMap(s._1)(1)).min)
    selected.map { _._1 }
  }

  /**
   * Compute reference lines, distances, and associate points to references
   * @param points
   * @param references
   * @param selectionIndices filter final point on these indices
   * @return map point i => ref point j,distance
   */
  def associateReferencePoints[I](points: Vector[Vector[Double]], references: Vector[Vector[Double]], population: Vector[I]): Map[I, (Int, Double)] = {
    val refnormsquared = references.map { _.map { x => x * x }.sum }
    // FIXME unoptimized, shouldnt recreate the matrices at each run
    def proj(dim: Int, x: Vector[Double]): Vector[Double] = {
      val w = MatrixUtils.createColumnRealMatrix(references(dim).toArray)
      w.multiply(MatrixUtils.createRowRealMatrix(x.toArray)).multiply(w).getColumn(0).map { _ / refnormsquared(dim) }.toVector
    }
    points.zip(population).map {
      case (p, ind) =>
        val dists = (0 until references.length).map {
          i =>
            math.sqrt(p.zip(proj(i, p)).map { case (x, y) => (x - y) * (x - y) }.sum)
        }
        val mindist = dists.min
        (ind, (dists.zipWithIndex.filter { case (d, _) => d == mindist }.map { case (_, j) => j }.head, mindist))
    }.toMap
  }

  def pointsSelection[I](associationMap: Map[I, (Int, Double)], selected: Vector[(I, Int)], toselect: Int)(implicit rng: util.Random): (Map[I, (Int, Double)], Vector[(I, Int)]) = {
    //println("Selecting "+toselect+" points from "+associationMap.toVector.size)
    toselect match {
      case n if n == 0 => (associationMap, selected)
      case _ => {
        //val refCount = selected.groupBy(_._2).map{g => (g._1,g._2.size)}
        //val refCount = associationMap.toVector.groupBy(_._2._1).map{g => (g._1,g._2.size)} // ref with no count can not be in the refcount
        val selectedRefCount = selected.groupBy(_._2).map { g => (g._1, g._2.size) }
        val refCount = associationMap.map { _._2._1 }.toVector.distinct.map { j => (j, selectedRefCount.getOrElse(j, 0)) }.toMap
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

