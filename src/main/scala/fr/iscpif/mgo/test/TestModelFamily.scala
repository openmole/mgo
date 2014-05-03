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

package fr.iscpif.mgo.test

import fr.iscpif.mgo.{ Population, Individual }
import fr.iscpif.mgo._
import scalaz.Lens
import scala.util.Random
import fr.iscpif.mgo.modelfamily.{ ModelFamilyGenome, ModelFamilyCrossover, ModelFamilyMutation, ModelFamilyElitism }

object TestModelFamily extends App {

  implicit val rng = new Random(42)

  val m = new RastriginVector with Evolution with ModelFamilyElitism with ModelFamilyMutation with ModelFamilyCrossover with NoArchive with RankModifier with MaxAggregation with GeneticBreeding with BinaryTournamentSelection with TournamentOnRank with HierarchicalRanking with ModelFamilyGenome with CounterTermination {
    override def genomeSize: Int = 5

    /** Number of steps before the algorithm stops */
    override def steps: Int = 1000

    /** the size of the offspring */
    override def lambda: Int = 100

    override def nicheSize: Int = 10

    override def models: Int = 50
  }

  println(m.masks)

  val res =
    m.evolve.untilConverged {
      s =>
        println(s.generation)
        println(m.niches(s.individuals).map { case (i, idv) => i -> idv.map(i => m.aggregate(i.fitness)).sorted.headOption })
    }.individuals

}
