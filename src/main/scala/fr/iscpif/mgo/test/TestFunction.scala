/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.algorithm.GAGenomeWithSigma
import fr.iscpif.mgo.ga.algorithm.GAGenomeWithSigmaFactory
import fr.iscpif.mgo.ga.algorithm.NSGAII
import fr.iscpif.mgo.ga.selection._
import fr.iscpif.mgo.tools.Scaling._
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection.Ranking._
import java.util.Random

object TestFunction extends App {
 def f(x: Double) = x * x
  
  def scale(x: Double) = x.scale(-100, 100)
    
  def evaluator(g: GAGenomeWithSigma) = 
    new Individual[GAGenomeWithSigma, GAFitness]{
      val genome = g
      val fitness = new GAFitness {
        def values = IndexedSeq(math.abs(4 - f(scale(g.values(0)))))
      }
    }
 
  implicit val rng = new Random
  val factory = new GAGenomeWithSigmaFactory(1)

  val nsga2 =
    NSGAII.sigma(
      _maxSameIndividual = 25,
      _maxStep = 50,
      _archiveSize = 100,
      _factory = factory,
      _evaluator = evaluator,
      _sbxDistributionIndex = 2,
      _rank = new ParetoCrowdingRank,
      _dominance = new StrictDominant
    )

  import nsga2._

  val popInitiale = (0 until 50).map {i => evaluator(factory.random)}
  val res = nsga2.evolveRun(popInitiale)

  val ranks = new ParetoRank().apply(res, new StrictDominant)
  val firstRank = ranks zip res sortBy (_._1.rank) foreach { case(i, r) => println(i.rank + " " + r.genome.values) }
  
  //, new RankPareto, new StrictDominant).foreach{i => println(scale(i.genome.values(0)))}

}
