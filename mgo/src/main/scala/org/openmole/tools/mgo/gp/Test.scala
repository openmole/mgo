/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import org.openmole.tools.mgo._
import gp._
import gp.operators._
import scala.io.Source

object Test extends App {
  implicit val aprng = new java.util.Random
  val funs  : IndexedSeq [ExprFactory] = IndexedSeq (Sum, Sub, Prod, Div, Sin, Cos)
  val terms : IndexedSeq [ExprFactory] = IndexedSeq (Num, new VarFactory ("t"))
  val data  : List [(Double, Double)]  = 
    Source.fromFile ("/iscpif/users/cardoso/data.txt").getLines map {l =>
      (l.split(';')(0).toDouble, l.split (';') (1).toDouble)} toList 
  val operators = 
    List (HoistMutation, new ShrinkMutation (terms), 
          new SubtreeMutation (terms, funs), SubtreeCrossOver)
  
  def operate (genomes : List [Expr]) : Expr = 
    operators (aprng.nextInt (operators.size)) operate (genomes.toIndexedSeq)
  
  def fitness (e : Expr) : Double =
    data.par map (x => scala.math.pow (x._2 - e.eval (Map ("t" -> x._1)), 2)) sum
  
  def evolve (pop : List [Individual [Expr]]) : List [Individual [Expr]] = {
    println ("Generating new population ...")
    val newPop : List [Individual [Expr]] = List.fill (pop.size) {
      val g = operate (pop map (_.genome))
      new Individual (g, fitness (g))
    }
    println ("Done generating new population")
    (newPop ++ pop) sortBy (_.fitness) take (pop.size)
  }
  
  
  def launch (oldPop : List [Individual [Expr]]) : List [Individual [Expr]] = {
    val newPop = evolve (oldPop)
    println ("Best individual : " + newPop (0))
    launch (newPop)
  }
  
  def generateRandomIndividual = {
    val e = ExprGenerator.genExpr (funs, terms, 6, "full")
    new Individual (e, fitness (e))
  }

  launch (List.fill (128) (generateRandomIndividual))
}
