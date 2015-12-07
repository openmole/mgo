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

import fr.iscpif.mgo.fitness._
import fr.iscpif.mgo.tools.Lazy

import scala.language.higherKinds
import scalaz.Ordering.{ LT, EQ, GT }
import scalaz._
import Scalaz._

import Breedings._
import ranking._
import diversity._

import scala.math.{ min, max }

import scala.util.Random

object Algorithms {

  object NSGA2 {

    def crossovers[M[_]: Monad](useRG: M[Random]): Vector[Crossover[(Vector[Double], Vector[Double]), M, (Vector[Double], Vector[Double])]] =
      Vector(
        replicatePairC(blxC(useRG)(0.1)),
        replicatePairC(blxC(useRG)(0.5)),
        replicatePairC(blxC(useRG)(2.0)),
        sbxC(useRG)(0.1),
        sbxC(useRG)(0.5),
        sbxC(useRG)(2.0)
      )

    def mutations[M[_]: Monad](useRG: M[Random]): Vector[Mutation[Vector[Double], M, Vector[Double]]] =
      Vector(
        bgaM(useRG)(mutationRate = 1.0 / _, mutationRange = 0.001),
        bgaM(useRG)(mutationRate = 1.0 / _, mutationRange = 0.01),
        bgaM(useRG)(mutationRate = 2.0 / _, mutationRange = 0.1),
        bgaM(useRG)(mutationRate = _ => 0.5, mutationRange = 0.5)
      )

    def crossoversAndMutations[M[_]: Monad](useRG: M[Random]): Vector[((Vector[Double], Vector[Double])) => M[(Vector[Double], Vector[Double])]] =
      for {
        c <- crossovers(useRG)
        m <- mutations(useRG)
      } yield {
        (mates: (Vector[Double], Vector[Double])) =>
          for {
            crossed <- c(mates)
            m1 <- m(crossed._1)
            m2 <- m(crossed._2)
          } yield (m1, m2)
      }

    def breeding[M[_]: Monad](useRG: M[Random])(
      lambda: Int,
      fitness: Fitness[Vector[Double], Seq[Double]],
      operationExploration: Double = 0.1): Breeding[(Vector[Double], Int), M, (Vector[Double], Int)] =
      (individuals: Vector[(Vector[Double], Int)]) => {

        type V = Vector[Double]
        type IWithOp = (V,Int)
        type GWithOp = (V,Int)

        for {
          rg <- useRG
          selected <- tournament[IWithOp, (Lazy[Int], Lazy[Double]), M](useRG)(
            rankAndDiversity(
              reversedRanking(paretoRanking[IWithOp] { (opWithI: IWithOp) => fitness(opWithI._1) }),
              crowdingDistance[IWithOp] { (opWithI: IWithOp) => fitness(opWithI._1) }(rg)),
            lambda)(implicitly[Order[(Lazy[Int], Lazy[Double])]], implicitly[Monad[M]])(individuals)
          bred <- dynamicallyOpB[V, M, V, (V, V), (V, V)](useRG)(
            pairConsecutive[V, M],
            { case (g1, g2) => Vector(g1, g2).point[M] },
            crossoversAndMutations[M](useRG),
            operationExploration)(implicitly[Monad[M]])(selected)
          clamped = (bred: Vector[GWithOp]).map { case (g,op) => (g.map { x: Double => max(0.0, min(1.0, x)) }, op) }
        } yield clamped
      }

    def elitism[M[_]: Monad]: Objective[M, (Int, Vector[Double])] = ???
    /*(individuals: Vector[(Int, Vector[Double])]) =>
        for {
          //p2 = applyCloneStrategy(p1, youngest)
          //p3 = removeNaN(p2, fitness)
          //p4 <- random[Unit] lifts keepNonDominated(mu, ranking, diversity)(p3)
        } yield */

    def step[M[_]: Monad]: Vector[(Int, Vector[Double])] => M[Vector[(Int, Vector[Double])]] = ???
    /*stepEA(
        ???,
        breeding,
        ???,
        elitism,
        muPlusLambda[(Int,Vector[Double])]) */

  }

}
