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
import scala.util.Random
import fr.iscpif.mgo.modelfamily.{ ModelFamilyGenome, ModelFamilyMutation, ModelFamilyElitism }
import tools.Math._

object TestModelFamily extends App {

  implicit val rng = new Random(42)

  val testModels = 1

  val m = new RastriginVector with Evolution with ModelFamilyElitism with ModelFamilyMutation with SBXCrossover with NoArchive with MaxAggregation with GeneticBreeding with BinaryTournamentSelection with TournamentOnRank with HierarchicalRanking with NoDiversity with ModelFamilyGenome with CounterTermination with ClampedGenome {
    /** Number of steps before the algorithm stops */
    override def steps: Int = 10000

    /** the size of the offspring */
    override def lambda: Int = 1000

    override def nicheSize: Int = 100

    override def modelMasks = ((1024 - testModels) until 1024)

    /*override def terminated(population: => Population[G, P, F, MF], step: STATE): (Boolean, STATE) = {
      val avgError =
        (population.toIndividuals.map {
          idv =>
            val i = niche(idv)
            idv.map(i => aggregate(idv.fitness)).sorted.headOption.getOrElse(Double.PositiveInfinity)
        }.sum - bestFitness.sum) / testModels
      val (term, s) = super.terminated(population, step)
      (term || avgError < 0.01, s)
    }*/
  }

  println(m.masks.map(_.count(_ == true)))

  val replications = 30

  val serie = (for {
    r <- (0 until replications)
  } yield m.evolve.untilConverged {
    s => println(s"$r  ${s.generation}")
  }.generation.toDouble)

  println(average(serie) + " " + mse(serie))

}
