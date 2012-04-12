package fr.iscpif.mgo.ga.algorithm

import annotation.tailrec
import fr.iscpif.mgo.ga.selection.{Ranking, Distance}
import fr.iscpif.mgo.ga.termination.AbstractTermination
import fr.iscpif.mgo.ga.{GAGenomeFactory, GAGenome, GAFitness}
import fr.iscpif.mgo.{Selection, CrossOver, Mutation, Individual}
import java.util.Random

/*
 * Copyright (C) 2011 srey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
trait AbstractAlgorithm[ G <: GAGenome, F <: GAGenomeFactory[G]] {

  type I <: Individual[G,GAFitness]

  def maxStep: Int
  def factory: F
  def evaluator: G => GAFitness

  def mutationOperator: Mutation[G, F]
  def crossoverOperator: CrossOver[G, F]
  def termination : AbstractTermination[I]

  def selection: Selection[I]

  //Perform N step
  @tailrec private def evolveStep(population: IndexedSeq[I], step: Int = 0)(implicit aprng:Random):IndexedSeq[I]= {
    if (step >= maxStep) population
    else {
      val nextPop = evolve(population)
      if (!termination.hasNext(population, nextPop)) evolveStep(nextPop, 0)
      else evolveStep(nextPop, step + 1)
    }
  }
  def evolve(population: IndexedSeq[I])(implicit aprng: Random): IndexedSeq[I]


}
