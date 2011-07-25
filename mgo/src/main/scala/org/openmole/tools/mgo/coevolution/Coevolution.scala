/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.coevolution

import java.util.Random
import org.openmole.tools.mgo._
import ga._
import gp._
import scala.math.pow

class Ga (
  params : Seq [String], 
  val popSize : Int, 
  fitness : GAGenome => Double, 
  val operators : List [Operator [GAGenome, GAGenomeFactory [GAGenome]]]) {
  
  def evaluate (genome : GAGenome) = new Individual (genome, fitness (genome))

  def generateGenome (
    pop : List [Individual [GAGenome]], 
    moreOperators : List [Operator [GAGenome, GAGenomeFactory [GAGenome]]]) 
    (implicit rnd : Random) : GAGenome = {
    val op = (operators ++ moreOperators) 
    (op (rnd.nextInt (op.size))).operate (pop.toArray map (_.genome))
  }
}

class Gp (
  val popSize : Int, 
  fitness : (GAGenome, Iterable [IndexedSeq [Double]]) => Double,
  val operators : List [Operator [Expr, ExprFactory]]) {

  def evaluate (genome : GAGenome, optima : Iterable [IndexedSeq [Double]]) =
    new Individual (genome, fitness (genome, optima))
  
  def generateGenome (pop : List [Individual [Expr]]) (implicit rnd : Random) =
    operators (rnd.nextInt (operators.size)) operate (pop.toArray map (_.genome))
}

class Coevolution (
  ga : Ga, gp : Gp,
  gaSelect : List [Individual [GAGenome]] => List [GAGenome],
  gpSelect : List [Individual [Expr]] => List [Operator [GAGenome, GAGenomeFactory [GAGenome]]],
  fitOptima : (Expr, List [GAGenome]) => Double) {
  implicit val rnd = new Random
  
  def coevolve (
    gaBestPop : List [Individual [GAGenome]], 
    gpBestPop : List [Individual [Expr]], 
    injectedOperators : List [Operator [GAGenome, GAGenomeFactory [GAGenome]]]) = {
    val gaGenomes    =
      List.fill (ga.popSize) (ga.generateGenome (gaBestPop, injectedOperators))
    print ("GA : evaluating genomes... ")
    val gaNewPop     = gaGenomes map ga.evaluate
    println ("done")
    print ("GA : generating new population... ")
    val gaNewBestPop = 
      (gaBestPop ++ gaNewPop).sortWith (_.fitness < _.fitness) take (ga.popSize)
    println ("done")
    val optima = gaSelect (gaNewBestPop)
    
    val gpOldPop     = gpBestPop map (i => new Individual (i.genome, Double.MaxValue))
    val gpGenomes    = List.fill (gp.popSize) (gp.generateGenome (gpOldPop))
    print ("GP : generating new population... ")
    val gpNewPop     = 
      gpGenomes map (g => new Individual (g, fitOptima (g, optima)))
    println ("done")
    val gpNewBestPop =
      (gpOldPop ++ gpNewPop).sortWith (_.fitness < _.fitness) take (gp.popSize)
    
    (gaNewBestPop, gpNewBestPop)
  }
}

