/*
 * Copyright (C) Guillaume Chérel 06/05/14
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._

import fr.iscpif.mgo._
import util.Random
import scalax.io.Resource
import scala.math._

object TestBehaviourSearchHitMap extends App {

  implicit val rng = new Random

  val m = new BehaviourSearchHitMap {

    override def genomeSize: Int = 2

    def min = Seq.fill(genomeSize)(0.0)
    def max = 1.0 :: List.fill(genomeSize)(5.0)

    /** ZDT4 functions **/
    def f1(x: Seq[Double]) = x(0)
    def f2(x: Seq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
    def g(x: Seq[Double]) =
      1 + 10 * (genomeSize - 1) + (1 until genomeSize).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum

    /** Number of steps before the algorithm stops */
    override def steps = 100

    /** the size of the offspring */
    override def lambda = 3

    override type P = Seq[Double]
    override def express(g: G, rng: Random): P = Vector(f1(g.values), f2(g.values))

    override type Niche = Seq[Int]
    override val keepN = 1
    val divsSize = 0.1
    override def niche(individual: Individual[G, P, F]): Niche =
      scale(individual.phenotype).map((x: Double) => (x / divsSize).toInt).toSeq
    def hitCell(i: Individual[G, P, F]) = niche(i)

  }

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/behaviourSearch/behaviourSearch${s.generation}.csv")
      output.append((0 until m.genomeSize).map("par" + _).mkString(",") + "," + (0 until 2).map("bhv" + _).mkString(",") + ",hitcounts,niche0,niche1" + "\n")
      val hitcounts = s.population.map(i => m.hits(s.archive, m.hitCell(i.toIndividual)))
      (s.population.content zip hitcounts).foreach {
        case (i, hc) => output.append(i.genome.values.mkString(",") + "," + i.phenotype.mkString(",") + "," + hc + "," + m.niche(i.toIndividual).mkString(",") + "\n")
      }
      println("step " + s.generation + " popsize " + s.population.content.size + " volume discovered " + s.archive.count { case (k, v) => s.archive.contains(k) && (s.archive(k) > 0) })
  }

}
