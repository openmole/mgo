/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.coevolution

import java.util.Random
import org.openmole.tools.mgo.gp._
import scala.math.pow

class Ga [T] (params : Seq [String], val popSize : Int, 
              fitness : T => Double, val operators : List [Operator [T]]) {
  
  def evaluate (genome : T) = new Individual (genome, fitness (genome))

  def generateGenome (pop : Iterable [Individual [T]], 
                      moreOperators : List [Operator [T]]) (implicit rnd : Random) : T = {
    val op = (operators ++ moreOperators) 
    (op (rnd.nextInt (op.size))).generateGenome(pop)
  }
}

class Gp [T] (val popSize : Int, fitness : (T, Iterable [IndexedSeq [Double]]) => Double,
              val operators : List [Operator [T]]) {

  def evaluate (genome : T, optima : Iterable [IndexedSeq [Double]]) = {
    new Individual (genome, fitness (genome, optima))
  } 
  
  def generateGenome (pop : Iterable [Individual [T]]) (implicit rnd : Random) =
    operators (rnd.nextInt (operators.size)) generateGenome (pop)
}

class Coevolution [A, P] (ga : Ga [A], gp : Gp [P],
                          gaSelect : List [Individual [A]] => List [A],
                          gpSelect : List [Individual [P]] => List [Operator [A]],
                          fitOptima : (P, List [A]) => Double) {
  implicit val rnd = new Random
  
  def coevolve (gaBestPop : List [Individual [A]], 
                gpBestPop : List [Individual [P]], 
                injectedOperators : List [Operator [A]]) = {
    val gaGenomes    =
      List.fill (ga.popSize) (ga.generateGenome (gaBestPop, injectedOperators))
    print ("GA : evaluating genomes... ")
    val gaNewPop     = gaGenomes map ga.evaluate
    println ("done")
    print ("GA : generating new population... ")
    val gaNewBestPop = 
      (gaBestPop ++ gaNewPop).sortWith (_.fit < _.fit) take (ga.popSize)
    println ("done")
    val optima = gaSelect (gaNewBestPop)
    
    val gpOldPop     = gpBestPop map (i => new Individual (i.genome, Double.MaxValue))
    val gpGenomes    = List.fill (gp.popSize) (gp.generateGenome (gpOldPop))
    print ("GP : generating new population... ")
    val gpNewPop     = 
      gpGenomes map (g => new Individual (g, fitOptima (g, optima)))
    println ("done")
    val gpNewBestPop =
      (gpOldPop ++ gpNewPop).sortWith (_.fit < _.fit) take (gp.popSize)
    
    (gaNewBestPop, gpNewBestPop)
  }
}

