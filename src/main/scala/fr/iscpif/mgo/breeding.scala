/*
 * Copyright (C) 04/12/2015 Guillaume Chérel
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
package fr.iscpif.mgo

import fr.iscpif.mgo.tools.Math._
import fr.iscpif.mgo.tools._

import scala.language.higherKinds

import scalaz._
import Scalaz._

import scala.util.Random

import Contexts._

object breeding {

  //type Breeding[I, M[_], G] = Vector[I] => M[Vector[G]]
  type Breeding[M[_], I, G] = Kleisli[M, Vector[I], Vector[G]]

  object Breeding {
    def apply[M[_]: Monad, I, G](f: Vector[I] => M[Vector[G]]): Breeding[M, I, G] = Kleisli.kleisli[M, Vector[I], Vector[G]](f)
  }

  //implicit def breedingToFunction[I, M[_]: Monad, G](b: Breeding[I, M, G]): Vector[I] => M[Vector[G]] = b.run
  //implicit def functionToBreeding[I, M[_]: Monad, G](f: Vector[I] => M[Vector[G]]): Breeding[I, M, G] = Kleisli.kleisli[M, Vector[I], Vector[G]](f)

  /*implicit def breedingMonad[I, M[_]: Monad]: Monad[BreedingMonad[I, M]#l] = new Monad[BreedingMonad[I, M]#l] {
    def point[A](a: => A): Breeding[I, M, A] = { _ => a.point[M] }
    def bind[A, B](fa: Breeding[I, M, A])(f: (A) ⇒ Breeding[I, M, B]): Breeding[I, M, B] = ???
  }*/

  /**** Selection ****/

  def tournament[M[_], I, K: Order](ranking: Kleisli[M, Vector[I], Vector[K]], size: Int, rounds: Int => Int = _ => 1)(implicit MM: Monad[M], MR: RandomGen[M]): Breeding[M, I, I] =
    Breeding.apply(
      (individuals: Vector[I]) =>
        if (individuals.isEmpty) individuals.point[M]
        else {
          val popsize = individuals.size

          def findOneChampion(scores: Vector[K], rg: Random): I = {
            val challengers: Vector[Int] = Vector.fill(rounds(popsize))(rg.nextInt(popsize))
            challengers.maximumBy(scores) match {
              case Some(champion) => individuals(champion)
              case None => individuals(rg.nextInt(popsize))
            }
          }

          for {
            rg <- MR.random
            scores <- ranking(individuals)
            champions = Vector.fill(size)(findOneChampion(scores, rg))
          } yield champions
        }
    )

  /**** Mating ****/

  def groupConsecutive[M[_]: Monad, I](groupSize: Int): Breeding[M, I, Vector[I]] =
    Breeding((individuals: Vector[I]) => individuals.grouped(groupSize).toVector.point[M])

  def pairConsecutive[M[_]: Monad, I]: Breeding[M, I, (I, I)] =
    Breeding((individuals: Vector[I]) => individuals.grouped(2).map { case Vector(a, b) => (a, b); case Vector(a) => (a, a) }.toVector.point[M])

  /**** Crossover ****/

  /**
   * A crossover is a function from some individuals (parents or mates, can be a single individual, a pair or a vector)
   * to one or more genomes (even if more than one, all genomes are coming from crossing over the same parents).
   *
   * The type P represent the parents, typically a single individual, a tuple, or a vector.
   *
   * The type G can also represent more than one genome.
   */
  type Crossover[M[_], P, O] = Kleisli[M, P, O]

  object Crossover {
    def apply[M[_]: Monad, P, O](f: P => M[O]): Crossover[M, P, O] = Kleisli.kleisli[M, P, O](f)
  }

  def replicateC[M[_]: Monad, P, O](n: Int, c: Crossover[M, P, O]): Crossover[M, P, Vector[O]] =
    Crossover((mates: P) =>
      for {
        gs <- c(mates).replicateM(n)
      } yield gs.toVector)

  def replicatePairC[M[_]: Monad, P, O](c: Crossover[M, P, O]): Crossover[M, P, (O, O)] =
    Crossover((mates: P) =>
      for {
        g1 <- c(mates)
        g2 <- c(mates)
      } yield (g1, g2))

  def identityC[M[_]: Monad, I]: Crossover[M, I, I] = Crossover(_.point[M])

  def blxC[M[_]](alpha: Double = 0.5)(implicit MM: Monad[M], MR: RandomGen[M]): Crossover[M, (Vector[Double], Vector[Double]), Vector[Double]] =
    Crossover((mates: (Vector[Double], Vector[Double])) =>
      for {
        rg <- MR.random
      } yield {
        (mates._1 zip mates._2).map {
          case (c1, c2) =>
            val cmin = math.min(c1, c2)
            val cmax = math.max(c1, c2)
            val range = cmax - cmin
            rg.nextDouble().scale(cmin - alpha * range, cmax + alpha * range)
        }
      })

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
  def sbxC[M[_]](distributionIndex: Double = 2.0)(implicit MM: Monad[M], MR: RandomGen[M]): Crossover[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])] =
    Crossover((mates: (Vector[Double], Vector[Double])) => {

      val exponent = 1.0 / (distributionIndex + 1.0)

      def elementCrossover(x0i: Double, x1i: Double)(rng: Random): (Double, Double) = {
        val u = rng.nextDouble

        val bq =
          if (u <= 0.5) math.pow(2 * u, exponent)
          else math.pow(1.0 / (2.0 * (1.0 - u)), exponent)

        val lb = 0.0
        val ub = 1.0
        val x0 = clamp(x0i, lb, ub)
        val x1 = clamp(x1i, lb, ub)
        val newX0 = 0.5 * ((1.0 + bq) * x0 + (1.0 - bq) * x1)
        val newX1 = 0.5 * ((1.0 - bq) * x0 + (1.0 + bq) * x1)
        (newX0, newX1)
      }

      val (g1, g2) = mates
      val zippedgs = g1 zip g2

      for {
        rng <- MR.random
      } yield {
        val (o1, o2): (Vector[Double], Vector[Double]) = zippedgs.map {
          case (g1e, g2e) => elementCrossover(g1e, g2e)(rng)
        }.unzip
        assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
        (o1, o2)
      }
    })

  /**** Mutation ****/

  /** A mutation is a function from a single genome to another single genome */
  type Mutation[M[_], G1, G2] = Kleisli[M, G1, G2]

  object Mutation {
    def apply[M[_]: Monad, G1, G2](f: G1 => M[G2]): Mutation[M, G1, G2] = Kleisli.kleisli[M, G1, G2](f)
  }

  def bgaM[M[_]](mutationRate: Int => Double, mutationRange: Double)(implicit MM: Monad[M], MR: RandomGen[M]): Mutation[M, Vector[Double], Vector[Double]] =
    Mutation((g: Vector[Double]) =>
      for {
        rng <- MR.random
      } yield {
        g.map {
          x =>
            if (rng.nextDouble < mutationRate(g.size)) {
              def alphai = if (rng.nextDouble < (1.0 / 16)) 1.0 else 0.0
              def ro = (0 to 15).map { i => alphai * math.pow(2, -i) }.sum
              def sign = if (rng.nextBoolean) 1.0 else -1.0
              x + (sign * mutationRange * ro)
            } else x
        }
      })

  //  def gaussianMutation[G, S](sigma: Double)(implicit values: monocle.Lens[G, Seq[Double] @@ genome.Value]): Mutation[G, S] = new Mutation[G, S] {
  //    override def apply(g: G) =
  //      for {
  //        rng <- random[S]
  //      } yield values.modify(g => g.map(_ + (rng.nextGaussian * sigma)))(g)
  //  }
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

  /**
   * Dynamically selects an operator and applies it for each pair of parents.
   *
   * @param mate Used to turn the individuals into a vector of elements that will be input to the selected operator.
   * @param unmate Used to turn a vector of elements output by the operator back into a vector of Gs.
   */
  def dynamicallyOpB[M[_], I, G, OI, OO](
    mate: Kleisli[M, Vector[I], Vector[OI]],
    unmate: Kleisli[M, OO, Vector[G]],
    ops: Vector[Kleisli[M, OI, OO]],
    exploration: Double)(implicit MM: Monad[M], MR: RandomGen[M]): Breeding[M, (I, Maybe[Int]), (G, Maybe[Int])] =
    Breeding((individuals: Vector[(I, Maybe[Int])]) => {
      val total = individuals.size * 2
      val proportion: Map[Int, Double] = individuals.collect { case (_, Maybe.Just(op)) => op }.groupBy(identity).mapValues(_.length.toDouble / total)

      def selectOp(rg: Random): Int =
        if (proportion.isEmpty || (rg.nextDouble < exploration)) rg.nextInt(ops.size)
        else multinomial(proportion.toList)(rg)

      for {
        vmates <- mate.run(individuals.map { _._1 })
        rg <- MR.random
        offspringsAndOp <- vmates.map { mates => (selectOp(rg), mates) }
          .traverse[M, (OO, Maybe[Int])] { case (selectedop, mates) => ops(selectedop).run(mates).map[(OO, Maybe[Int])] { (_: OO, Maybe.Just(selectedop)) } }
        bred <- offspringsAndOp.traverse[M, Vector[(G, Maybe[Int])]] { case (offsprings, op) => unmate.run(offsprings).map[Vector[(G, Maybe[Int])]] { (gs: Vector[G]) => gs.map((_: G, op)) } }
      } yield bred.toVector.flatten
    })

  /**** Cloning ****/
  /** Replaces randamly some of the genomes in gs by genomes taken from the original population of Is */
  def clonesReplace[M[_], I, G](cloneProbability: Double)(gs: Vector[G], gLens: I => G)(implicit MM: Monad[M], MR: RandomGen[M]): Breeding[M, I, G] =
    Breeding { is: Vector[I] =>
      val isSize = is.size
      for {
        rg <- MR.random
        result <- gs.traverse { (g: G) => if (rg.nextDouble < cloneProbability) gLens(is(rg.nextInt(isSize))).point[M] else g.point[M] }
      } yield result
    }

  //  def opOrClone[M[_]: Monad: RandomGen, I, G](
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
}
