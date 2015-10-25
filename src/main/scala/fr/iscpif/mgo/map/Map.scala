/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.mgo.map

import fr.iscpif.mgo._
import fr.iscpif.mgo.elitism.Niche
import fr.iscpif.mgo.ranking.HierarchicalRanking
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.tools.{Math, Lazy}
import fr.iscpif.mgo.tools.Math._
import fr.iscpif.mgo.{Pop, Population, Individual, Algorithm}
import monocle._
import fr.iscpif.mgo.genome._

import scala.util.Random

trait Map <: Pop with Ranking with Niche with Fitness {

  trait Plotter[T] {
    def apply(individual: Ind): T
  }

  def genomeProfilePlotter(x: Int, nX: Int)(implicit values: Lens[G, GenomeValue[Seq[Double]]]) =
    new Plotter[Int] {
      override def apply(individual: Ind): Int = {
        val niche = (values.get(individual.genome).value(x) * nX).toInt
        if (niche == nX) niche - 1 else niche
      }
    }

  def mapGenomePlotter (x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, GenomeValue[Seq[Double]]]) =
    new Plotter[(Int, Int)] {
      override def apply(i: Ind) = {
        val (nicheX, nicheY) = ((values.get(i.genome).value(x) * nX).toInt, (values.get(i.genome).value(y) * nY).toInt)
        (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
      }
    }

  def profileRanking(implicit plotter: Plotter[Int], aggregation: Fitness[Double]) = new Ranking {

    override def apply(population: Pop): Vector[Lazy[Int]] = {
      val (points, indexes) =
        population.map {
          i => (plotter(i).toDouble, aggregation(i))
        }.zipWithIndex.sortBy(_._1._1).unzip

      def signedSurface(p1: Point2D, p2: Point2D, p3: Point2D) = {
        val surface = Math.surface(p1, p2, p3)
        if (isUpper(p1, p3, p2)) -surface else surface
      }

      val contributions =
        points match {
          case Seq() => Seq.empty
          case Seq(x) => Seq(1.0)
          case s =>
            val first = s(0)
            val second = s(1)
            val zero = (first.x - (second.x - first.x), second.y)

            val leftSurface = signedSurface(zero, first, second)

            val preLast = s(s.length - 2)
            val last = s(s.length - 1)
            val postLast = (last.x + (last.x - preLast.x), preLast.y)

            val rightSurface = signedSurface(preLast, last, postLast)

            val middlePoints = s.sliding(3).filter(_.size == 3).map {
              s => signedSurface(s(0), s(1), s(2))
            }

            val surfaces = (Seq(leftSurface) ++ middlePoints ++ Seq(rightSurface)).zip(indexes).sortBy(_._2).map(_._1)
            val smallest = surfaces.min
            surfaces.map(s => s - smallest)
        }

      HierarchicalRanking.downRank(contributions.toVector)
    }
  }

  def profileNiche(nicheSize: Int = 1)(implicit plotter: Plotter[Int]) = new Niche[Int] {
    override def apply(individual: Ind): Int = plotter(individual)
  }

  def mapNiche(nicheSize: Int = 1)(implicit plotter: Plotter[(Int,Int)]) = new Niche[(Int, Int)] {
    override def apply(individual: Ind): (Int, Int) = plotter(individual)
  }


}
