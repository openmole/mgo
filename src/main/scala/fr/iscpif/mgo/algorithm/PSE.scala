/*
 * Copyright (C) 13/05/2014 Guillaume Chérel
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

/*trait PSE <: NoFitness
  with HitMapArchive
  with PhenotypeGridNiche
  with GeneticBreeding
  with BinaryTournamentSelection
  with RandomMating
  with DynamicGACrossover
  with DynamicGAMutation
  with GAGenomeWithSigma
  with TournamentOnHitCount
  with HierarchicalRanking
  with RandomNicheElitism
  with CounterTermination
  with ClampedGenome
  with ProportionalNumberOfRound
  with Cloning
  with RandomInitialGenome*/

trait PSE[Point] <: Algorithm with GeneticAlgorithm with AllFunctions {

  case class PSEState(hitMap: collection.Map[Point, Int])
  type STATE = PSEState
  def initialState = PSEState(collection.Map())

  implicit val hits = monocle.macros.Lenser[PSEState](_.hitMap)
  implicit val niche: Niche[Point]
  implicit val pointEqual = Equal.equal[Point](_ == _)

  def cloneRate = 0.0
  implicit val mergeClones = youngest

  override def breeding(pop: Pop): State[AlgorithmState, Vector[G]] =
    onHitCount.apply(pop) flatMap { challenged =>
      val fight = tournament(challenged, pop, size => math.round(math.log10(size).toInt))
      val newGenomes = breedGenomes(fight, crossover(pop), mutation(pop))
      interleaveClones(newGenomes.map(_.map(clamp)), fight.map(_.genome), cloneRate, lambda).map(_.toVector)
    }

  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
    for {
      p1 <- merge(population, offspring)
      p2 <- mergeClones(p1)
      p3 <- nicheElitism(keepRandom(1), p2)
    } yield age(p3)

}