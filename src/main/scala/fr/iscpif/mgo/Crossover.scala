/*
 * Copyright (C) 2012 reuillon
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

import scala.util.Random
import tools._
import scalaz._
import Scalaz._
import Genome._
import tools.Math._

trait Crossover <: Pop { this: Algorithm =>
  type Crossover = ((G, G) => State[AlgorithmState, (G, G)])
}


trait CrossoverFunctions <: Crossover with DynamicOps { this: Algorithm =>

  def identityCrossover = new Crossover {
    override def apply(g1: G, g2: G): State[AlgorithmState, (G, G)] = State.state((g1, g2))
  }

  def blx(alpha: Double = 0.5)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], random: monocle.Lens[AlgorithmState, Random]) = new Crossover {
    def apply(g1: G, g2: G) =
      for {
        random <- get[AlgorithmState].map(random.get)
      } yield {
        val (newG1, newG2) =
          (values.get(g1) zip values.get(g2)).map {
            case (c1, c2) =>
              val cmin = math.min(c1, c2)
              val cmax = math.max(c1, c2)
              val i = cmax - cmin
              def generate = random.nextDouble().scale(cmin - alpha * i, cmax + alpha * i)
              (generate, generate)
          }.unzip
        (values.set(newG1)(g1), values.set(newG2)(g2))
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
  def sbx(distributionIndex: Double = 2.0)(implicit values: monocle.Lens[G, Seq[Double] @@ Genome.Value], random: monocle.Lens[AlgorithmState, Random]) = new Crossover {

    def crossover(g1: Seq[Double], g2: Seq[Double])(rng: Random): (Seq[Double], Seq[Double]) = {

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

      (g1 zip g2).map {
        case (g1e, g2e) => elementCrossover(g1e, g2e)(rng)
      }.unzip
    }


    def apply(g1: G, g2: G) =
      for {
        random <- get[AlgorithmState].map(random.get)
      } yield {
        val (o1, o2) = crossover(values.get(g1), values.get(g2))(random)
        assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
        (values.set(o1)(g1), values.set(o2)(g2))
      }

  }


  def dynamicCrossover(genomePart: monocle.Lens[G, Option[Int]], exploration: Double = 0.1)(ops: Crossover*)(implicit random: monocle.Lens[AlgorithmState, Random]) = (pop: Pop) => new Crossover {
     def apply(g1: G, g2: G) =
       for {
         s <- random.lifts(dynamicOperator(pop, genomePart, exploration, ops.zipWithIndex.toVector))
         (crossover, i) = s
         List(dyg1, dyg2) = List(g1, g2).map(genomePart.set(Some(i)))
         res <- crossover(dyg1, dyg2)
       } yield res
  }


}