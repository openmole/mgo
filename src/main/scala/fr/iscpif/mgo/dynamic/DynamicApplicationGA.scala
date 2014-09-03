/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.dynamic

import fr.iscpif.mgo._
import tools._
import monocle._
import monocle.syntax._
import collection.{ Map => SMap }
import scala.util.Random

trait DynamicApplicationGA <: Crossover with Mutation with DynamicApplicationGAGenome with GeneticBreeding { self =>

  def operatorExploration: Double = 0.1

  trait CandidateCrossover <: Crossover {
    type G = self.G
    type P = self.P
    type F = self.F
    type A = self.A
  }

  trait CandidateMutation <: Mutation {
    type G = self.G
    type P = self.P
    type F = self.F
    type A = self.A
  }

  trait DAMethodInjection {
    def randomGenome(implicit rng: Random): G = self.randomGenome
    def rawValues: SimpleLens[G, Seq[Double]] = self.rawValues
    def sigma: SimpleLens[G, Seq[Double]] = self.sigma
    def genomeSize: Int = self.genomeSize
    def clamp(values: SimpleLens[G, Seq[Double]]): SimpleLens[G, Seq[Double]] = self.clamp(values)
  }

  lazy val sbx = new SBXCrossover with CandidateCrossover with DAMethodInjection {}
  lazy val blx = new BLXCrossover with CandidateCrossover with DAMethodInjection {}
  lazy val idc = new IdentityCrossOver with CandidateCrossover with DAMethodInjection {}

  lazy val bga = new BGAMutation with CandidateMutation with DAMethodInjection {}
  lazy val bigbga = new BGAMutation with CandidateMutation with DAMethodInjection {
    override def mutationRange: Double = 0.5
    override def mutationRate: Double = 0.5
  }

  lazy val adaptiveCauchy = new AdaptiveCauchyMutation with CandidateMutation with DAMethodInjection {}

  def crossovers: Seq[CandidateCrossover] = IndexedSeq(sbx, blx)
  def mutations: Seq[CandidateMutation] = IndexedSeq(bga, adaptiveCauchy, bigbga)

  private def select(workingStats: SMap[Int, Double], size: Int)(implicit rng: Random) = {
    lazy val all = (0 until size)
    def roulette(weights: List[(Int, Double)], selected: Double): Int =
      weights match {
        case Nil => all.random
        case (i, p) :: t =>
          if (selected <= p) i
          else roulette(t, selected - p)
      }
    if (rng.nextDouble < operatorExploration) all.random
    else roulette(workingStats.toList, rng.nextDouble)
  }

  override def crossover(g1: G, g2: G, population: Population[G, P, F], archive: A)(implicit rng: Random) = {
    def crossoverStats(p: Population[G, P, F]) = {
      val working = p.flatMap(_.genome |-> crossover get)
      working.groupBy(identity).mapValues(_.size.toDouble / working.size)
    }

    val i = select(crossoverStats(population), crossovers.size)
    crossovers(i).crossover(g1, g2, population, archive).map(g => g |-> crossover set Some(i))
  }

  override def mutate(g: G, population: Population[G, P, F], archive: A)(implicit rng: Random): G = {
    def mutationStats(p: Population[G, P, F]) = {
      val working = p.flatMap(_.genome |-> mutation get)
      working.groupBy(identity).mapValues(_.size.toDouble / working.size)
    }

    val i = select(mutationStats(population), mutations.size)
    mutations(i).mutate(g, population, archive) |-> mutation set Some(i)
  }

  override def breed(i1: Individual[G, P, F], i2: Individual[G, P, F], population: Population[G, P, F], a: A)(implicit rng: Random): Seq[G] =
    super.breed(i1, i2, population, a).
      map { g => g |-> ancestors set Some((i1.fitness, i2.fitness)) }
}
