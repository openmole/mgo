/*
 * Copyright (C) 07/02/14 Romain Reuillon
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._

import util.Random
import scalax.io.Resource

object TestNovelty extends App {
  val m =
    new ZDT4 with IndividualDiversityModifier with PhenotypeIsPosition with ClosedCrowdingIndividualDistance with ClosedCrowdingIndividualDistanceFromArchive with NoveltyArchive with NonDominatedElitism with MG with GAGenomeWithSigma with ClosedCrowdingDiversity with GeneticBreeding with BinaryTournamentSelection with TournamentOnRankAndDiversity with CoEvolvingSigmaValuesMutation with SBXBoundedCrossover with StrictDominance with CounterTermination with ParetoRanking with ClampedGenome {

      override def genomeSize = 10

      override def archiveEpsilon = 0.001

      /** the size of the population */
      override def mu = 100

      /** Number of steps before the algorithm stops */
      override def steps = 100

      /** the size of the offspring */
      override def lambda = 100

    }

  implicit val rng = new Random

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/novelty/novelty${s.generation}.csv")
      s.archive.foreach {
        i => output.append(m.values.get(i.genome).mkString(",") + "," + m.fitness(i).mkString(",") + "\n")
      }
      println(s.individuals.map(i => m.fitness(i).max).min)
    //println(s.generation + " " + s.archive.size)
  }

}
