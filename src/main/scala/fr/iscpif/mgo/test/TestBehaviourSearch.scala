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
import scala.math._

/* TODO: on dirait que l'élitisme par niche n'est pas pris en compte
 * TODO: calcul de diversité n'est pas bon
 */

object TestBehaviourSearch extends App {

  implicit val rng = new Random

  val m =
    new GAProblem with NoFitness with NoArchive with NoveltyModifier with GeneticBreeding with TournamentOnRankAndDiversity with IdentityCrossOver with PickNNicheElitism with SortedTournamentSelection with ClosedCrowdingDiversity with ClosedCrowdingIndividualDistance with StrictDominance with CounterTermination with GaussianMutation with GAGenome {

      override def genomeSize = 2

      def min = Seq.fill(genomeSize)(0.0)
      def max = 1.0 :: List.fill(genomeSize)(5.0)

      def f1(x: Seq[Double]) = x(0)
      def f2(x: Seq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
      def g(x: Seq[Double]) =
        1 + 10 * (genomeSize - 1) + (1 until genomeSize).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum

      /** Number of steps before the algorithm stops */
      override def steps = 100

      /** the size of the offspring */
      override def lambda = 3

      /** std of the gaussian mutation */
      override def sigma = 0.1

      override type P = Seq[Double]
      override def express(g: G, rng: Random): P = Vector(f1(g.values), f2(g.values))

      //def individualDistance(g: Seq[Individual[G, P, F]]): Seq[Lazy[Double]] =
      override def individualPosition(individual: Individual[G, P, F]): Seq[Double] =
        individual.phenotype

      override type Niche = Seq[Int]
      override val keepN = 1
      val divsSize = 0.1
      override def individualToNiche(individual: Individual[G, P, F]): Niche =
        scale(individual.phenotype).map((x: Double) => (x / divsSize).toInt).toSeq

    }

  m.evolve.untilConverged {
    s =>
      val output = Resource.fromFile(s"/tmp/behaviourSearch/behaviourSearch${s.generation}.csv")
      output.append((0 until m.genomeSize).map("par" + _).mkString(",") + "," + (0 until 2).map("bhv" + _).mkString(",") + ",diversity,niche0,niche1" + "\n")
      s.population.content.foreach {
        i => output.append(i.genome.values.mkString(",") + "," + i.phenotype.mkString(",") + "," + m.diversity.get(i.metaFitness)().toString + "," + m.individualToNiche(i.toIndividual).mkString(",") + "\n")
      }
      println("step " + s.generation + " popsize " + s.population.content.size + " volume discovered " + s.population.toIndividuals.groupBy(m.individualToNiche).size)
  }

}
