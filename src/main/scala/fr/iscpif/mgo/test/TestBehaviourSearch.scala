/*
 * Copyright (C) Guillaume Chérel 21/04/14
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

import util.Random
import scalax.io.Resource

object TestBehaviourSearch {

  implicit val rng = new Random

  val m =
    new ZDT4 with NoFitness with NoArchive with NoveltyModifier with GeneticBreeding with TournamentOnRankAndDiversity with IdentityCrossOver with PickNNicheElitism with SortedTournamentSelection with ClosedCrowdingDiversity with ClosedCrowdingIndividualDistance with ClosedCrowdingIndividualDistanceFromArchive with StrictDominance with CounterTermination with GaussianMutation with GAGenome {

      override def genomeSize = 10

      /** Number of steps before the algorithm stops */
      override def steps = 100

      /** the size of the offspring */
      override def lambda = 50

      /** std of the gaussian mutation */
      override def sigma = 0.1

      override type P = Seq[Double]
      override def express(g: G): P = Vector(f1(g.values), f2(g.values))

      //def individualDistance(g: Seq[Individual[G, P, F]]): Seq[Lazy[Double]] =
      override def individualPosition(individual: Individual[G, P, F]): Seq[Double] =
        individual.phenotype

      override type Niche = Seq[Int]
      override val keepN = 1
      val divsPerDim = 10
      override def individualToNiche(individual: Individual[G, P, F]): Niche =
        scale(individual.phenotype).map((x: Double) => (x * divsPerDim).toInt).toSeq

    }

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/novelty/novelty${s.generation}.csv")
      s.archive.foreach {
        i => output.append(i.genome.values.mkString(",") + "," + i.fitness.values.mkString(",") + "\n")
      }
      println(s.individuals.map(_.fitness.values.max).min)
    //println(s.generation + " " + s.archive.size)
  }

}
