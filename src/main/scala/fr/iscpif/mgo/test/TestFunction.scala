/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.tools.Scaling._
import fr.iscpif.mgo.selection._
import java.util.Random

object TestFunction extends App { self =>
  def f(x: Double) = x * x
  
  def scale(x: Double) = x.scale(-100, 100)
    
  def evaluator(g: GAGenomeWithSigma) = 
    new Fitness {
      def values = IndexedSeq(math.abs(4 - f(scale(g.values(0)))))
    }
 
  implicit val rng = new Random

  val nsga2 =
    new NSGAII.NSGAIISigma(
      distributionIndex = 2,
      steadySince = 1000,
      archiveSize = 50,
      evaluator = self.evaluator,
      genomeSize = 1
    )

  val res = nsga2.run(50)
  val ranks = nsga2.rank(res)
  val firstRank = ranks zip res sortBy (_._1.rank) foreach { case(i, r) => println(i.rank + " " + r.genome.values) }
  
}
