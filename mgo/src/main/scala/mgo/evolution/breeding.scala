package mgo.evolution

import cats.*
import cats.data.*
import cats.implicits.*
import mgo.tools
import mgo.tools.*
import mgo.tools.execution.*

import scala.annotation.tailrec
import scala.reflect.ClassTag


object breeding {

  type Breeding[S, I, G] = (S, Vector[I], scala.util.Random) => Vector[G]

  //  object Breeding {
  //    def apply[S, I, G](f: (S, Vector[I]) => Vector[G]): Breeding[S, I, G] =
  //      Kleisli[State[S, ?], Vector[I], Vector[G]](i => State.pure(f(i)))
  //  }

  //implicit def breedingToFunction[I, M[_]: cats.Monad, G](b: Breeding[I, M, G]): Vector[I] => M[Vector[G]] = b.run
  //implicit def functionToBreeding[I, M[_]: cats.Monad, G](f: Vector[I] => M[Vector[G]]): Breeding[I, M, G] = Kleisli.kleisli[M, Vector[I], Vector[G]](f)

  /*implicit def breedingcats.Monad[I, M[_]: cats.Monad]: cats.Monad[Breedingcats.Monad[I, M]#l] = new cats.Monad[Breedingcats.Monad[I, M]#l] {
    def point[A](a: => A): Breeding[I, M, A] = { _ => a.point[M] }
    def bind[A, B](fa: Breeding[I, M, A])(f: (A) ⇒ Breeding[I, M, B]): Breeding[I, M, B] = ???
  }*/

/**** Selection ****/

  type Selection[S, I] = (S, Vector[I], scala.util.Random) => I

  def tournament[S, I, K: Order as ko](
    ranks: Vector[K],
    rounds: Int => Int = _ => 1): Selection[S, I] =
    (s, individuals, rng) =>
      val populationSize = individuals.size
      val challengersIndices = Vector.fill(rounds(populationSize) + 1)(() => rng.nextInt(populationSize)).map(_())
      individuals(challengersIndices.maxBy(i => ranks(i))(using ko.toOrdering))

  def log2(x: Int): Int = (math.log(x) / math.log(2)).toInt
  lazy val log2_256: Int = log2(256)

  /* 1 round from 0 to 256 and then 1 more round for each population doubling */
  def logOfPopulationSize(size: Int): Int =
    math.max(log2(size) - log2_256 + 2, 1)

  def randomSelection[S, I]: Selection[S, I] =
    (s, population, rng) => {
      val i = rng.nextInt(population.size)
      population(i)
    }

/**** Mating ****/

  //  def groupConsecutive[M[_]: cats.Monad, I](groupSize: Int): Breeding[M, I, Vector[I]] =
  //    (individuals: Vector[I]) => individuals.grouped(groupSize).toVector.pure[M]

  //  def pairConsecutive[M[_]: cats.Monad, I] =
  //    (individuals: Vector[I]) => individuals.grouped(2).collect { case Vector(a, b) => (a, b) }.toVector

/**** Crossover ****/

  /**
   * A crossover is a function from some individuals (parents or mates, can be a single individual, a pair or a vector)
   * to one or more genomes (even if more than one, all genomes are coming from crossing over the same parents).
   *
   * The type P represent the parents, typically a single individual, a tuple, or a vector.
   *
   * The type G can also represent more than one genome.
   */

  type Crossover[S, P, O] = (S, P, scala.util.Random) => O
  type GACrossover[S, T] = Crossover[S, (IArray[T], IArray[T]), (IArray[T], IArray[T])]


  def blxC[S](alpha: Double = 0.5): Crossover[S, (IArray[Double], IArray[Double]), IArray[Double]] =
    (s, mates, random) =>
      (mates._1 zip mates._2).map: (c1, c2) =>
        val cmin = math.min(c1, c2)
        val cmax = math.max(c1, c2)
        val range = cmax - cmin
        random.nextDouble.scale(cmin - alpha * range, cmax + alpha * range)


  /**
   * SBX RGA operator with Bounded Variable modification, see APPENDIX A p30 into :
   *
   * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7291&rep=rep1&type=pdf
   *
   * INPROCEEDINGS{Deb98anefficient,
   *   author = {Kalyanmoy Deb},
   *   title = {An Efficient Constraint Handling Method for Genetic Algorithms},
   *   booktitle = {Computer Methods in Applied Mechanics and Engineering},
   *   year = {1998},
   *   pages = {311--338}
   * }
   *
   * Notes : Deb implementation differs from NSGA2 he proposed on this site :
   * http://www.iitk.ac.in/kangal/codes.shtml
   *
   * Implementation based on http://repository.ias.ac.in/9415/1/318.pdf
   *
   */
  def sbxC[S](distributionIndex: Double = 2.0): GACrossover[S, Double] = (s, mates, random) =>
    val exponent = 1.0 / (distributionIndex + 1.0)

    def elementCrossover(x0i: Double, x1i: Double): (Double, Double) =
      val u = random.nextDouble
      val bq =
        if u <= 0.5
        then Math.pow(2 * u, exponent)
        else Math.pow(1.0 / (2.0 * (1.0 - u)), exponent)

      val lb = 0.0
      val ub = 1.0

      val x0 = clamp(x0i, lb, ub)
      val x1 = clamp(x1i, lb, ub)

      val newX0 = 0.5 * ((1.0 + bq) * x0 + (1.0 - bq) * x1)
      val newX1 = 0.5 * ((1.0 - bq) * x0 + (1.0 + bq) * x1)

      (newX0, newX1)

    val (g1, g2) = mates

    val size = Math.min(g1.length, g2.length)
    val o1 = Array.ofDim[Double](size)
    val o2 = Array.ofDim[Double](size)

    loop(
      0,
      _ < size,
      _ + 1
    ): i =>
      val (e1, e2) = elementCrossover(g1(i), g2(i))
      o1(i) = e1
      o2(i) = e2


    (IArray.unsafeFromArray(o1), IArray.unsafeFromArray(o2))


/**** Mutation ****/

  /** A mutation is a function from a single genome to another single genome */
  type Mutation[S, G1, G2] = (S, G1, scala.util.Random) => G2
  type GAMutation[S] = Mutation[S, IArray[Double], IArray[Double]]

  //  def bga[M[_]: cats.Monad: Random](mutationRate: Int => Double, mutationRange: Double): Mutation[M, Vector[Double], Vector[Double]] =
  //    Mutation { (g: Vector[Double]) =>
  //      def alphai = Random[M].nextDouble.map(d => if (d < (1.0 / 16)) 1.0 else 0.0)
  //      def roi(i: Int) = alphai.map(_ * math.pow(2, -i))
  //
  //      g.traverse { x =>
  //
  //        Random[M].nextDouble.map(_ < mutationRate(g.size)).flatMap { mutate =>
  //          if (mutate)
  //            for {
  //              ro <- (0 to 15).toVector.traverse { roi }.map(_.sum)
  //              r <- Random[M].nextDouble().map(_ * mutationRange)
  //              sign <- Random[M].nextBoolean.map(b => if (b) 1.0 else -1.0)
  //            } yield x + (sign * r * ro)
  //          else x.pure[M]
  //        }
  //      }
  //    }

  def gaussianMutation[S](mutationRate: Int => Double, sigma: Double): GAMutation[S] =
    (s, g, random) =>
      g.map { x =>
        val mutate = random.nextDouble < mutationRate(g.size)
        if (mutate) {
          val s = random.nextGaussian() * sigma
          x + s
        } else x
      }

  //
  //  /**
  //   * Mutation of a genome based on gaussian distribution around the genome with adaptive sigma values.
  //   * See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
  //   * See on paper : Gaussian mutation and self adaptation (Hinterding) &&
  //   * Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert
  //   * Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It,
  //   * Modern Heuristics
  //   */
  //  def adaptiveCauchy[G, S](minimumSigma: Double = 1e-30)(implicit values: monocle.Lens[G, Seq[Double] @@ genome.Value], sigma: monocle.Lens[G, Seq[Double] @@ Sigma]): Mutation[G, S] = new Mutation[G, S] {
  //
  //    override def apply(g: G) =
  //      for {
  //        rng <- random[S]
  //      } yield {
  //        val newSigma = sigma.get(g).map { s => math.max(minimumSigma, s * math.exp(rng.nextGaussian)) }
  //
  //        val newValues =
  //          (values.get(g) zip newSigma) map {
  //            case (v, s) => new CauchyDistribution(rng, v, s).sample //.nextGaussian * s + v
  //          }
  //
  //        newValues.foreach(v => assert(!v.isNaN))
  //
  //        (values.set(newValues) compose sigma.set(newSigma))(g)
  //      }
  //
  //  }
  //
  //  /**
  //   * Polynomial mutationolynomial mutation by Deb and Goyal. If is the value of
  //   * the ith parameter selected for mutation with a probability pm and the result
  //   * of the mutation is the new value obtained by a polynomial probability
  //   * distribution.
  //   * Based on the source code of Jmetal library
  //   * Author : Antonio J. Nebro <antonio@lcc.uma.es> and Juan J. Durillo <durillo@lcc.uma.es>
  //   */
  //  def polynomial[G, S](distributionIndex: Double, mutationRate: Double)(implicit values: monocle.Lens[G, Seq[Double] @@ genome.Value]): Mutation[G, S] = new Mutation[G, S] {
  //    override def apply(g: G) =
  //      for {
  //        rng <- random[S]
  //      } yield {
  //        val newValues = values.get(g) map {
  //          v =>
  //            if (rng.nextDouble <= mutationRate) {
  //              val yl = 0.0 // lower bound
  //              val yu = 1.0 // upper bound
  //              val delta1 = (v - yl) / (yu - yl)
  //              val delta2 = (yu - v) / (yu - yl)
  //              val mut_pow = 1.0 / (distributionIndex + 1.0)
  //              val rnd = rng.nextDouble
  //
  //              val deltaq: Double = (if (rnd <= 0.5) {
  //                val xy = 1.0 - delta1
  //                val value = 2.0 * rnd + (1.0 - 2.0 * rnd) * (math.pow(xy, (distributionIndex + 1.0)))
  //                math.pow(value, mut_pow) - 1.0
  //              } else {
  //                val xy = 1.0 - delta2
  //                val value = 2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * (math.pow(xy, (distributionIndex + 1.0)))
  //                1.0 - (math.pow(value, mut_pow))
  //              })
  //
  //              val finalValue = v + deltaq * (yu - yl)
  //
  //              if (finalValue < yl) yl
  //              else if (finalValue > yu) yu
  //              else finalValue
  //            }
  //            v
  //        }
  //
  //        values.set(newValues)(g)
  //      }
  //  }

/**** Dynamic breeding ****/

  //  def breed[OI, OO](
  //    tournament: Kleisli[State[util.Random, ?], Vector[OI], OI],
  //    op: Kleisli[State[util.Random, ?], (OI, OI), Vector[OO]],
  //    size: Int) = Kleisli[State[util.Random, ?], Vector[OI], Vector[OO]] { ois =>
  //
  //    def pair =
  //      for {
  //        i1 <- tournament(ois)
  //        i2 <- tournament(ois)
  //      } yield (i1, i2)
  //
  //    def generateGenome = pair flatMap { i => op(i) }
  //
  //    def pop(current: Vector[OO], rng: util.Random): Vector[OO] =
  //      if (current.size >= size) current
  //      else pop(current ++ generateGenome.eval(rng), rng)
  //
  //    for {
  //      rng <- State.get[util.Random]
  //    } yield pop(Vector.empty, rng)
  //  }

  /** Randomly replaces some of the genomes in gs by genomes taken from the original population of I */
  def clonesReplace[S, I, G](cloneProbability: Double, population: Vector[I], genome: I => G, selection: Selection[S, I]): Breeding[S, G, G] = (s, gs, rng) =>
    gs.map: g =>
      val clone = rng.nextDouble < cloneProbability
      if clone
      then genome(selection(s, population, rng))
      else g



  //  def opOrClone[M[_]: cats.Monad: RandomGen, I, G](
  //    clone: I => G,
  //    op: I => M[G],
  //    cloneProbability: Double): Kleisli[M, I, G] =
  //    /*for {
  //      _ <- probabilisticOperatorB[ M,I, G](
  //        Vector(
  //          (Kleisli.kleisli[M,I,G]{i: I => clone(i).point[M]}, cloneProbability),
  //          (Kleisli.kleisli[M,I,G]{op(_)}, 1 - cloneProbability)))
  //      res <- Kleisli.kleisli[M, (G, Int), G]{case (i, _) => i.point[M]}
  //    } yield res*/
  //    probabilisticOperatorB[M, I, G](
  //      Vector(
  //        (Kleisli.kleisli[M, I, G] { i: I => clone(i).point[M] }, cloneProbability),
  //        (Kleisli.kleisli[M, I, G] { op(_) }, 1 - cloneProbability))) >=>
  //      Kleisli.kleisli[M, (G, Int), G] { case (i, _) => i.point[M] }

  /* ----------------- Discrete operators ----------------------- */

  def randomMutation[S](mutationRate: Int => Double, discrete: Vector[D]): Mutation[S, IArray[Int], IArray[Int]] =
    (s, values, rng) =>
      val size = discrete.size
      val r = mutationRate(size)
      val res = Array.ofDim[Int](size)

      loop(0, _ < size, _ + 1): i =>
        if rng.nextDouble() < r
        then res(i) = tools.randomInt(rng, discrete(i))
        else res(i) = values(i)

      IArray.unsafeFromArray(res)

  def binaryCrossover[S, V: ClassTag](rate: Int => Double): GACrossover[S, V] = (s, v, rng) =>
    val size = v._1.size
    val r = rate(size)
    val r1 = Array.ofDim[V](size)
    val r2 = Array.ofDim[V](size)

    loop(0, _ < size, _ + 1): i =>
      if rng.nextDouble() < r
      then
        r2(i) = v._1(i)
        r1(i) = v._2(i)
      else
        r1(i) = v._1(i)
        r2(i) = v._2(i)

    (IArray.unsafeFromArray(r1), IArray.unsafeFromArray(r2))

  /* tool functions */

  def breed[S, I, G](breeding: Breeding[S, I, G], lambda: Int, reject: Option[G => Boolean] = None): Breeding[S, I, G] =
    val rejectValue = reject.getOrElse(_ => false)
    (s, population, rng) =>
      @tailrec def accumulate(acc: Vector[G] = Vector()): Vector[G] =
        if acc.size >= lambda
        then acc
        else
          val b = breeding(s, population, rng).filter(s => !rejectValue(s))
          accumulate(b ++ acc)

      def randomTake[G](gs: Vector[G], lambda: Int, random: scala.util.Random): Vector[G] = random.shuffle(gs).take(lambda)

      val generated = accumulate()
      randomTake(generated, lambda, rng)

}
