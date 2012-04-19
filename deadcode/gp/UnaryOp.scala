/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.gp

abstract class UnaryOp extends Expr {
  val _1 : Expr
  val subtrees : List [Expr] = List (_1)
  lazy val depth = 1 + _1.depth
  lazy val size  = 1 + _1.size
  val arity = 1
  
  def subtreesAtDepth (d : Int) : List [Expr] = 
    if (d == 0) List (this)
    else _1.subtreesAtDepth (d - 1)
  
  def getSubtree (i : Int) = {
    if (i < 0 || i >= size) sys.error (this + " : No such node at index " + i)
    if (i == 0) this
    else _1.getSubtree (i-1)
  }
  
  /** Return the depth of the node at index i **/
  def getDepth (i : Int) : Int = {
    if (i < 0 || i >= size) sys.error (this + " : No such node at index " + i)
    if (i == 0) 0
    else 1 + _1.getDepth (i - 1)
  }
  
  override def replaceSubtreeWith (i : Int, by : Expr) : Expr = {
    if (i < 0 || i >= this.size) sys.error (this + " : No such node at index " + i)
    if (i == 0) by
    else companion (_1.replaceSubtreeWith (i - 1, by))
  }
  
  /** Simplifiy the expression if possible **/
  override def simplify = companion (_1.simplify)
  
}

trait UnaryOpFactory [T <: UnaryOp] extends ExprFactory {
  val arity = 1
}