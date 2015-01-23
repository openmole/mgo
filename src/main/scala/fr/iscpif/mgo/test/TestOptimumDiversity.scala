/*
 * Copyright (C) 13/11/13 Romain Reuillon
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
import fr.iscpif.mgo.diversity.GenomeDiversity
import fr.iscpif.mgo.ranking.{ CeilDiversityRanking, DiversityRanking }
import monocle.syntax._
import scala.util.Random
import scalax.io.Resource

object TestOptimumDiversity extends App {

  /* trait OptimumDiversity <: Evolution
    with BinaryTournamentSelection
    with TournamentOnRankAndDiversity
    with NonDominatedElitism
    with DynamicApplicationGA
    with DiversityRanking
    with FitnessCrowdingDiversity
    with CeilDiversityRanking
    with MaxAggregation
    with NonStrictDominance
    with NoArchive
    with CloneRemoval
    with GeneticBreeding
    with MGFitness
    with ClampedGenome*/

  /*trait OptimumDiversity <: NoArchive
    with BestRankedNicheElitism
    with ParetoRanking
    with GAGenotypeGridNiche
    with MG
    with FitnessCrowdingDiversity
    with GeneticBreeding
    with BinaryTournamentSelection
    with TournamentOnRankAndDiversity
    with DynamicApplicationGA
    with NonStrictDominance
    with ClampedGenome
    with ProportionalNumberOfRound

  val m = new Rastrigin with OptimumDiversity with CounterTermination {
    def genomeSize: Int = 2
    def lambda: Int = 200
    //def mu: Int = 200
    def steps = 1000
    def gridSize = Seq(0.25, 0.25)
  }

  implicit val rng = new Random

  m.evolve.untilConverged { s =>
    val output = Resource.fromFile(s"/tmp/novelty/novelty${s.generation}.csv")
    s.population.foreach {
      i => output.append((m.scale(i.genome |-> m.values get)).mkString(",") + "," + i.fitness.mkString(",") + "\n")
    }
  }*/

  /*val m = new ZDT4 with OptimumDiversity with CounterTermination {
    def genomeSize: Int = 2
    def lambda: Int = 200
    def steps = 400
    def gridSize = Seq(0.1, 0.5, 0.5)
  }

  implicit val rng = new Random

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/novelty/novelty${s.generation}.csv")
      s.population.foreach {
        i => output.append((m.scale(i.genome |-> m.values get)).mkString(",") + "," + i.fitness.mkString(",") + "\n")
      }
      //println(s.individuals.map(_.fitness))

      //import Ordering.Implicits._
      //println(s.individuals.map(m.niche).sorted.mkString("\n"))

      //println(s.individuals.map(i => m.niche(i).mkString(",")).mkString("\n"))

      //println(s.individuals.size)
      println(s.generation)
  }*/

}
