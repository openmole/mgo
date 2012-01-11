/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import org.openmole.tools.mgo.Genome

trait Expr extends Genome {
  val companion : ExprFactory
  val subtrees : List [Expr]
  val depth : Int
  val size  : Int
  val arity : Int
  
  def eval (env : Map [String, Double]) : Double
  
  def subtreesAtDepth (d : Int) : List [Expr]
  
  def getSubtree (i : Int) : Expr
  
  /** Return the depth of the node at index i **/
  def getDepth (i : Int) : Int
  
  def replaceSubtreeWith (i : Int, by : Expr) : Expr
  
  /** Simplifiy the expression if possible **/
  def simplify : Expr
  def toString : String
  def pretty : String = toString
}