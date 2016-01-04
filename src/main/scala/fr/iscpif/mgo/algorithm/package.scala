/*
 * Copyright (C) 2015 Guillaume Chérel, Romain Reuillon
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
package fr.iscpif.mgo

import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.Contexts._
import fr.iscpif.mgo.tools._

import scala.util.Random

import scalaz._
import Scalaz._
import monocle.Lens

package object algorithm {

  def randomTakeLambda[M[_], G](lambda: Int)(implicit MR: RandomGen[M], MM: Monad[M]) =
    Breeding[M, G, G] { gs: Vector[G] =>
      MR.random.map { _.shuffle(gs).take(lambda) }
    }

  def operatorProportions[M[_]: Monad, I](operation: I => Maybe[Int]) =
    Kleisli.kleisli[M, Vector[I], Map[Int, Double]] {
      is: Vector[I] =>
        is.map { operation }.collect { case Maybe.Just(op) => op }.groupBy(identity).mapValues(_.length.toDouble / is.size).point[M]
    }

  def selectOperator[M[_]: Monad, G](operators: Vector[Kleisli[M, G, G]], opStats: Map[Int, Double], exploration: Double)(implicit MR: RandomGen[M]) = {
    def allOps =
      operators.zipWithIndex.map {
        case (op, index) => (op, opStats.getOrElse(index, 0.0))
      }

    probabilisticOperatorB[M, G](allOps, exploration)
  }

  def probabilisticOperatorB[M[_], G](opsAndWeights: Vector[(Kleisli[M, G, G], Double)], exploration: Double)(implicit MM: Monad[M], MR: RandomGen[M]): Kleisli[M, G, (G, Int)] =
    Kleisli((mates: G) => {
      for {
        rg <- MR.random
        op = if (rg.nextDouble < exploration) rg.nextInt(opsAndWeights.size)
        else multinomial[Int](opsAndWeights.zipWithIndex.map { case ((op, w), i) => (i, w) }.toList)(rg)
        g <- opsAndWeights(op)._1.run(mates)
      } yield (g, op)
    })

  object GenomeVectorDouble {
    def randomGenomes[M[_]](n: Int, genomeLength: Int)(
      implicit MM: Monad[M], MR: RandomGen[M]): M[Vector[Vector[Double]]] =
      for {
        rg <- MR.random
        values = Vector.fill(n)(Vector.fill(genomeLength)(rg.nextDouble))
      } yield values

    def clamp[M[_]: Monad, G](lens: monocle.Lens[G, Vector[Double]]) = mapPureB[M, G, G] {
      lens.modify(_ map { x: Double => math.max(0.0, math.min(1.0, x)) })
    }

    def filterNaN[M[_]: Monad, I](genomeValues: I => Vector[Double]) =
      flatMapPureB[M, I, I] { i => if (genomeValues(i).exists { _.isNaN }) Vector.empty else Vector(i) }

    def crossovers[M[_]: Monad: RandomGen]: Vector[Crossover[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
      Vector(
        replicatePairC(blxC(0.1)),
        replicatePairC(blxC(0.5)),
        replicatePairC(blxC(2.0)),
        sbxC(0.1),
        sbxC(0.5),
        sbxC(2.0)
      )

    def mutations[M[_]: Monad: RandomGen]: Vector[Mutation[M, Vector[Double], Vector[Double]]] =
      Vector(
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.001),
        bgaM(mutationRate = 1.0 / _, mutationRange = 0.01),
        bgaM(mutationRate = 2.0 / _, mutationRange = 0.1),
        bgaM(mutationRate = _ => 0.5, mutationRange = 0.5)
      )

    def crossoversAndMutations[M[_]: Monad: RandomGen]: Vector[Kleisli[M, (Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
      for {
        c <- crossovers[M]
        m <- mutations[M]
      } yield {
        Kleisli((mates: (Vector[Double], Vector[Double])) =>
          for {
            crossed <- c.run(mates)
            m1 <- m.run(crossed._1)
            m2 <- m.run(crossed._2)
          } yield (m1, m2))
      }

    def applyDynamicOperator[M[_]: Monad: RandomGen](operatorStatistics: Map[Int, Double], operatorExploration: Double) =
      mapB[M, (Vector[Double], Vector[Double]), ((Vector[Double], Vector[Double]), Int)] { g =>
        selectOperator[M, (Vector[Double], Vector[Double])](
          crossoversAndMutations[M],
          operatorStatistics,
          operatorExploration
        ).run(g)
      }
  }

  object dynamicOperators {

  }
}
