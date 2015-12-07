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

import scala.annotation.tailrec

import scala.language.higherKinds

import scalaz._
import Scalaz._

import scala.util.Random

object Breedings {

  /**** Selection ****/

  def tournament[I, K: Order, M[_]: Monad](useRG: M[Random])(ranking: Vector[I] => Vector[K], size: Int, rounds: Int => Int = _ => 1): Breeding[I, M, I] =
    (individuals: Vector[I]) =>
      if (individuals.isEmpty) individuals.point[M]
      else {
        val scores: Vector[K] = ranking(individuals)
        val popsize = individuals.size

        def findOneChampion(rg: Random): I = {
          val challengers: Vector[Int] = Vector.fill(rounds(popsize))(rg.nextInt(popsize))
          challengers.maximumBy(scores) match {
            case Some(champion) => individuals(champion)
            case None => individuals(rg.nextInt(popsize))
          }
        }

        for {
          rgs <- useRG.replicateM(size)
          champions = rgs.toVector.map(findOneChampion)
        } yield champions
      }

  /**** Mating ****/

  def groupConsecutive[I, M[_]: Monad](groupSize: Int): Breeding[I, M, Vector[I]] =
    (individuals: Vector[I]) => individuals.grouped(groupSize).toVector.point[M]

  def pairConsecutive[I, M[_]: Monad]: Breeding[I, M, (I, I)] =
    (individuals: Vector[I]) => individuals.grouped(2).map { case Vector(a, b) => (a, b); case Vector(a) => (a, a) }.toVector.point[M]

  /**** Crossover ****/

  /**
   * A crossover is a function from some individuals (parents or mates, can be a single individual, a pair or a vector)
   * to one or more genomes (even if more than one, all genomes are coming from crossing over the same parents).
   *
   * The type P represent the parents, typically a single individual, a tuple, or a vector.
   *
   * The type G can also represent more than one genome.
   */
  type Crossover[P, M[_], G] = P => M[G]

  def replicateC[P, M[_]: Monad, G](n: Int, c: Crossover[P, M, G]): Crossover[P, M, Vector[G]] =
    (mates: P) =>
      for {
        gs <- c(mates).replicateM(n)
      } yield (gs.toVector)

  def replicatePairC[P, M[_]: Monad, G](c: Crossover[P, M, G]): Crossover[P, M, (G, G)] =
    (mates: P) =>
      for {
        g1 <- c(mates)
        g2 <- c(mates)
      } yield (g1, g2)

  def identityC[I, M[_]: Monad]: Crossover[I, M, I] = _.point[M]

  def blxC[M[_]: Monad](useRG: M[Random])(alpha: Double = 0.5): Crossover[(Vector[Double], Vector[Double]), M, Vector[Double]] =
    (mates: (Vector[Double], Vector[Double])) =>
      for {
        rg <- useRG
      } yield {
        (mates._1 zip mates._2).map {
          case (c1, c2) =>
            val cmin = math.min(c1, c2)
            val cmax = math.max(c1, c2)
            val range = cmax - cmin
            rg.nextDouble().scale(cmin - alpha * range, cmax + alpha * range)
        }
      }

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
  def sbxC[M[_]: Monad](useRG: M[Random])(distributionIndex: Double = 2.0): Crossover[(Vector[Double], Vector[Double]), M, (Vector[Double], Vector[Double])] =
    (mates: (Vector[Double], Vector[Double])) => {

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
        rgs <- useRG.replicateM(zippedgs.size)
      } yield {
        val (o1, o2): (Vector[Double], Vector[Double]) = (zippedgs zip rgs).map {
          case ((g1e, g2e), rg) => elementCrossover(g1e, g2e)(rg)
        }.unzip
        assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
        (o1, o2)
      }
    }

  /**** Mutation ****/

  /** A mutation is a function from a single genome to another single genome */
  type Mutation[G1, M[_], G2] = G1 => M[G2]

  def bgaM[M[_]: Monad](useRG: M[Random])(mutationRate: Int => Double, mutationRange: Double): Mutation[Vector[Double], M, Vector[Double]] =
    (g: Vector[Double]) =>
      for {
        rng <- useRG
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
      }

  /**** Dynamic breeding ****/

  /** Dynamically selects an operator and applies it for each pair of parents.
    *
    * @param mate Used to turn the individuals into a vector of elements that will be input to the selected operator.
    * @param unmate Used to turn a vector of elements output by the operator back into a vector of Gs. */
  def dynamicallyOpB[I, M[_]: Monad, G, OI, OO](useRG: M[Random])(
      mate: Vector[I] => M[Vector[OI]],
      unmate: OO => M[Vector[G]],
      ops: Vector[OI => M[OO]],
      exploration: Double): Breeding[(I,Int), M, (G,Int)] =
    (individuals: Vector[(I,Int)]) => {
      val total = individuals.size * 2
      val proportion: Map[Int, Double] = individuals.map(_._2).groupBy(identity).mapValues(_.length.toDouble / total)

      def selectOp(rg: Random): Int =
        if (rg.nextDouble < exploration) rg.nextInt(ops.size)
        else multinomial(proportion.toList)(rg)

      for {
        vmates <- mate(individuals.map{_._1})
        rgs <- useRG.replicateM(vmates.size)
        offspringsAndOp <- (rgs.toVector zip vmates).map { case (rg, mates) => (selectOp(rg), mates) }
          .traverse[M, (OO, Int)] { case (selectedop, mates) => ops(selectedop)(mates).map[(OO, Int)] { (_: OO, selectedop) } }
        bred <- offspringsAndOp.traverse[M, Vector[(G,Int)]] { case (offsprings, op) => unmate(offsprings).map[Vector[(G,Int)]] { (gs: Vector[G]) => gs.map((_: G, op)) } }
      } yield bred.toVector.flatten
    }

}
