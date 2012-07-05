/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
  implicit val factory = GAGenomeWithSigma.factory(2)
  
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
      
      override def stepListner(pop: P, state: STATE): Unit = println(state)
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
