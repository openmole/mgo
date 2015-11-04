/*
 * Copyright (C) 2012 Romain Reuillon
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

import org.apache.commons.math3.distribution.CauchyDistribution

import scala.language.higherKinds
import scala.util.Random
import scalaz._
import Scalaz._
import Genome._
import tools._

object Mutation {
  sealed trait Sigma
}

import Mutation._

trait Mutation <: Pop { this: Algorithm =>
  type Mutation = (G => State[AlgorithmState, G])
}

trait MutationFunctions <: Mutation with Genome with DynamicOps { this: Algorithm =>

  def identityMutation = new Mutation {
    override def apply(g: G): State[AlgorithmState, G] = State.state (g)
  }

  def gaussianMutation(sigma: Double)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], random: monocle.Lens[AlgorithmState, Random]) = new Mutation {
    override def apply(g: G) =
     for {
       rng <- random lifts get[Random]
     } yield values.modify(g => g.map(_ + (rng.nextGaussian * sigma)))(g)
  }

  /**
   * Mutation of a genome based on gaussian distribution around the genome with adaptive sigma values.
   * See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
   * See on paper : Gaussian mutation and self adaptation (Hinterding) &&
   * Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert
   * Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It,
   * Modern Heuristics
   */
  def adaptiveCauchy(minimumSigma: Double = 1e-30)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], sigma: monocle.Lens[G, Seq[Double] @@ Sigma], random: monocle.Lens[AlgorithmState, Random]) = new Mutation {

    override def apply(g: G) =
      for {
        rng <- random lifts get[Random ]
      } yield {
        val newSigma = sigma.get(g).map { s => math.max(minimumSigma, s * math.exp(rng.nextGaussian)) }

        val newValues =
          (values.get(g) zip newSigma) map {
            case (v, s) => new CauchyDistribution(rng, v, s).sample //.nextGaussian * s + v
          }

        newValues.foreach(v => assert(!v.isNaN))

        (values.set(newValues) compose sigma.set(newSigma)) (g)
      }

  }

  def bga(mutationRate: Int => Double, mutationRange: Double)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], random: monocle.Lens[AlgorithmState, Random]) = new Mutation {
    override def apply(g: G) =
      for {
        rng <- random lifts get[Random ]
      } yield {
        val vs = values.get(g)

        val newG = vs.map {
          g =>
            if (rng.nextDouble < mutationRate(vs.size)) {
              def alphai = if (rng.nextDouble < (1.0 / 16)) 1.0 else 0.0
              def ro = (0 to 15).map { i => alphai * math.pow(2, -i) }.sum
              def sign = if (rng.nextBoolean) 1.0 else -1.0
              g + (sign * mutationRange * ro)
            } else g
        }

        values.set(newG)(g)
      }
  }


  /**
   * Polynomial mutationolynomial mutation by Deb and Goyal. If is the value of
   * the ith parameter selected for mutation with a probability pm and the result
   * of the mutation is the new value obtained by a polynomial probability
   * distribution.
   * Based on the source code of Jmetal library
   * Author : Antonio J. Nebro <antonio@lcc.uma.es> and Juan J. Durillo <durillo@lcc.uma.es>
   */
  def polynomial(distributionIndex: Double, mutationRate: Double)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], random: monocle.Lens[AlgorithmState, Random]) = new Mutation {
    override def apply(g: G) =
      for {
        rng <- random lifts get[Random ]
      } yield {
        val newValues = values.get(g) map {
          v =>
            if (rng.nextDouble <= mutationRate) {
              val yl = 0.0 // lower bound
              val yu = 1.0 // upper bound
              val delta1 = (v - yl) / (yu - yl)
              val delta2 = (yu - v) / (yu - yl)
              val mut_pow = 1.0 / (distributionIndex + 1.0)
              val rnd = rng.nextDouble

              val deltaq: Double = (if (rnd <= 0.5) {
                val xy = 1.0 - delta1
                val value = 2.0 * rnd + (1.0 - 2.0 * rnd) * (math.pow(xy, (distributionIndex + 1.0)))
                math.pow(value, mut_pow) - 1.0
              } else {
                val xy = 1.0 - delta2
                val value = 2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * (math.pow(xy, (distributionIndex + 1.0)))
                1.0 - (math.pow(value, mut_pow))
              })

              val finalValue = v + deltaq * (yu - yl)

              if (finalValue < yl) yl
              else if (finalValue > yu) yu
              else finalValue
            }
            v
        }

        values.set(newValues)(g)
      }
  }

  def dynamicMutation(genomePart: monocle.Lens[G, Option[Int]], exploration: Double = 0.1)(ops: Mutation*)(implicit random: monocle.Lens[AlgorithmState, Random]) = (pop: Pop) => new Mutation {
     def apply(g: G) =
       for {
         s <- random lifts (dynamicOperator(pop, genomePart, exploration, ops.zipWithIndex.toVector))
         (mutation, i) = s
         res <- mutation(genomePart.set(Some(i))(g))
       } yield res
  }

}