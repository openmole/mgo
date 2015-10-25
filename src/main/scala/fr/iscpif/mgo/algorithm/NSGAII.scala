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
import fr.iscpif.mgo.breed.BreedingDefault
import fr.iscpif.mgo.diversity.{DiversityDefault, Diversity}
import fr.iscpif.mgo.elitism.ElitismDefault
import fr.iscpif.mgo.mutation.MutationDefault
import fr.iscpif.mgo.genome._
import fr.iscpif.mgo.ranking.{RankingDefault, Ranking}

import scalaz._
import Scalaz._

trait NSGAII <: Algorithm with ElitismDefault with BreedingDefault with MutationDefault with RankingDefault with DiversityDefault {

  case class Genome(values: GenomeValue[Seq[Double]])
  type G = Genome

  implicit def equalsG = Equal.equal[G]((g1, g2) => g1.values == g2.values)
  implicit def genomeValues = monocle.macros.Lenser[G](_.values)

  case class NSGA2State()

  type STATE = NSGA2State

  def lambda: Int

  implicit def fitness: Fitness[Seq[Double]]
  implicit def ranking = paretoRanking()
  implicit def diversity = crowdingDistance

  override def breeding(pop: Pop): State[EvolutionState, Vector[G]] = {
    (onRank and onDiversity) (pop) flatMap { challenged =>
        val newGenome =
          for {
            mated <- tournament(challenged, pop)()
            g <- gaussianMutation(2.0, mated.genome)
          } yield { g }

      (0 until lambda).toVector.traverseS[EvolutionState, G](i => newGenome)
    }
  }

  override def elitism(population: Pop, offspring: Pop): State[EvolutionState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- removeClone(p1)
    } yield p2

}