/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

abstract class BinaryOp extends Expr {
  val _1, _2 : Expr
  val subtrees : List [Expr] = List (_1, _2)
  lazy val depth = 1 + scala.math.max (_1.depth, _2.depth)
  lazy val size  = 1 + _1.size + _2.size
  val arity = 2
  
  def subtreesAtDepth (d : Int) : List [Expr] = 
    if (d == 0) List (this)
    else _1.subtreesAtDepth (d - 1) ::: _2.subtreesAtDepth(d - 1)
  
  def getSubtree (i : Int) = {
    if (i < 0 || i >= size) sys.error (this + " : No such node at index " + i)
    if (i == 0) this
    else
      if (i <= _1.size) _1.getSubtree (i - 1)
      else _2.getSubtree (i - _1.size - 1)
  }
  
  /** Return the depth of the node at index i **/
  def getDepth (i : Int) : Int = {
    if (i < 0 || i >= size) sys.error (this + " : No such node at index " + i)
    if (i == 0) 0
    else 
      if (i <= _1.size) 1 + _1.getDepth (i-1)
      else 1 + _2.getDepth (i-_1.size-1)
  }
  
  override def replaceSubtreeWith (i : Int, by : Expr) : Expr = {
    if (i < 0 || i >= this.size) sys.error (this + " : No such node at index " + i) 
    if (i == 0) by
    else 
      if (i <= _1.size) companion (_1.replaceSubtreeWith (i-1, by), _2)
      else companion (_1, _2.replaceSubtreeWith (i-_1.size-1, by))
  }
  
  /** Simplifiy the expression if possible **/
  override def simplify = companion (_1.simplify, _2.simplify)
}

trait BinaryOpFactory [T <: BinaryOp] extends ExprFactory {
  val arity = 2
}

