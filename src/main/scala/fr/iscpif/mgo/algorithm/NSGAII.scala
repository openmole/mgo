/*
 * Copyright (C) 22/05/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import scalaz._
import Scalaz._

trait NSGAII <: Algorithm with ElitismDefault with BreedingDefault with MutationDefault with CrossoverDefault with RankingDefault with DiversityDefault {

  case class Genome(values: GenomeValue[Seq[Double]], fromMutation: Option[Int] = None, fromCrossover: Option[Int] = None)
  type G = Genome

  implicit def equalsG = Equal.equal[G]((g1, g2) => g1.values == g2.values)
  implicit def genomeValues = monocle.macros.Lenser[G](_.values)

  def fromMutation = monocle.macros.Lenser[G](_.fromMutation)
  def fromCrossover = monocle.macros.Lenser[G](_.fromCrossover)

  case class NSGA2State()
  type STATE = NSGA2State
  def initialState = NSGA2State()

  def mutation = dynamicMutation(fromMutation)(gaussianMutation(0.2))
  def crossover = dynamicCrossover(fromCrossover)(blx())

  implicit def fitness: Fitness[Seq[Double]]
  implicit def ranking = paretoRanking()
  implicit def diversity = crowdingDistance

  override def breeding(pop: Pop): State[AlgorithmState, Vector[G]] = {
    (onRank and onDiversity) (pop) flatMap { challenged =>
        val newGenome =
          for {
            s1 <- tournament(challenged, pop)
            s2 <- tournament(challenged, pop)
            c <- crossover(pop)(s1.genome, s2.genome)
            (c1, c2) = c
            g1 <- mutation(pop)(c1)
            g2 <- mutation(pop)(c2)
          } yield { Vector(clamp(g2), clamp(g2)) }

        newGenome.generateFlat(lambda)
      }
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- removeClone(p1)
      p3 <- removeNaN(p2)
      p4 <- keepNonDominated(mu, p3)
    } yield p4.age

}