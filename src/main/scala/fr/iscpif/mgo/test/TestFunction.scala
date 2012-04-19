///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
//package fr.iscpif.mgo.test
//
//import fr.iscpif.mgo._
//import fr.iscpif.mgo.ga._
//import fr.iscpif.mgo.ranking._
//import fr.iscpif.mgo.elitism._
//import fr.iscpif.mgo.tools.Scaling._
//import fr.iscpif.mgo.domination._
//import fr.iscpif.mgo.selection._
//import java.util.Random
//
//object TestFunction extends App {
// def f(x: Double) = x * x
//  
//  def scale(x: Double) = x.scale(-100, 100)
//    
//  def evaluator(g: GAGenomeWithSigma) = 
//    new Individual[GAGenomeWithSigma, Fitness]{
//      val genome = g
//      val fitness = new Fitness {
//        def values = IndexedSeq(math.abs(4 - f(scale(g.values(0)))))
//      }
//    }
// 
//  implicit val rng = new Random
//  val factory = new GAGenomeWithSigmaFactory(1)
//
//  val nsga2 =
//    NSGAII.sigma(
//      _maxSameIndividual = 25,
//      _maxStep = 50,
//      _archiveSize = 100,
//      _factory = factory,
//      _evaluator = evaluator,
//      _sbxDistributionIndex = 2,
//      _rank = new ParetoCrowdingRank,
//      _dominance = new StrictDominant
//    )
//
//  import nsga2._
//
//  val popInitiale = (0 until 50).map {i => evaluator(factory.random)}
//  val res = nsga2.evolveRun(popInitiale)
//
//  val ranks = new ParetoRank().apply(res, new StrictDominant)
//  val firstRank = ranks zip res sortBy (_._1.rank) foreach { case(i, r) => println(i.rank + " " + r.genome.values) }
//  
//  //, new RankPareto, new StrictDominant).foreach{i => println(scale(i.genome.values(0)))}
//
//}
