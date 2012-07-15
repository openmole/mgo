/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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
import fr.iscpif.mgo.algorithm.ga._
import fr.iscpif.mgo.modifier._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.crossover._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.termination._
import fr.iscpif.mgo.tools.Scaling._
import fr.iscpif.mgo.selection._
import fr.iscpif.mgo.ga._

import java.util.Random

object TestFunction extends App { 
  def f(x: Double) = x * x
  
  def scale(x: Double) = x.scale(-100, 100)
    
  def evaluator(g: GAGenomeWithSigma) = 
    new Fitness {
      def values = IndexedSeq(
        math.abs(4 - f(scale(g.values(0)))),
        math.abs(4 - f(scale(g.values(1))))
      )
    }
 
  implicit val rng = new Random
  
  val nsga2 =
      new NSGAIISigma
                     with MGBinaryTournamentSelection
                     with CounterTermination
                     with NonDominatedSortingElitism
                     with CoEvolvingSigmaValuesMutation
                     with SBXBoundedCrossover 
                     with CrowdingDistance
                     with ParetoRanking
                     with StrictDominance
                     with RankDiversityGenomicCrowdingModifier {
      def distributionIndex = 2
      def maxStep = 10000
      def archiveSize = 50
      def genomeSize = 2
      
      override def stepListner(pop: Population[G, MF], state: STATE): Unit = println(state)
     /* override def stepListner(pop: P, state: STATE): Unit = {
        println(pop)
       /* val ranks = ParetoRanking(pop, this)
        ranks zip pop sortBy (_._1.rank) foreach { case(i, r) => println(i.rank + " " + r.genome.values + " " + r.fitness.values) }*/
        println("------------------------------------")
      }*/
      
    }
  
  val res = nsga2.run(50, evaluator _)
  val ranks = ParetoRanking(res.individuals, nsga2)
  ranks zip res sortBy (_._1) foreach { case(i, r) => println(i + " " + r.genome.values + " " + r.fitness.values) }
}
