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

import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Contexts._

import scala.util.Random

import scalaz._
import Scalaz._

package object algorithm {

  object GenomeVectorDouble {
    def randomGenomes[M[_]: Monad: RandomGen](n: Int, genomeLength: Int): M[Vector[Vector[Double]]] =
      for {
        rg <- implicitly[RandomGen[M]].split
        values = Vector.fill(n)(Vector.fill(genomeLength)(Random.nextDouble))
      } yield values
  }

  object dynamicOperators {
    def crossovers[M[_]: Monad: RandomGen]: Vector[Crossover[ M,(Vector[Double], Vector[Double]), (Vector[Double], Vector[Double])]] =
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

    def crossoversAndMutations[M[_]: Monad: RandomGen]: Vector[Kleisli[M,(Vector[Double], Vector[Double]),(Vector[Double], Vector[Double])]] =
      for {
        c <- crossovers[M]
        m <- mutations[M]
      } yield {
        Kleisli((mates: (Vector[Double], Vector[Double])) =>
          for {
            crossed <- c(mates)
            m1 <- m(crossed._1)
            m2 <- m(crossed._2)
          } yield (m1, m2))
      }
  }
}
