/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

trait NullaryOp extends Expr {
  val subtrees : List [Expr] = Nil
  val depth = 0
  val size  = 1
  val arity = 0
  
  def subtreesAtDepth (d : Int) : List [Expr] = 
    if (d == 0) List (this)
    else Nil
  
  def getSubtree (i : Int) =
    if (i == 0) this
    else sys.error (this + " : No such node at index " + i)
  
  /** Return the depth of the node at index i **/
  def getDepth (i : Int) : Int = 
    if (i == 0) 0
    else sys.error (this + " : No such node at index " + i)
  
  override def replaceSubtreeWith (i : Int, by : Expr) : Expr = 
    if (i == 0) by
    else sys.error (this + " : No such node at index " + i)
  
  /** Simplifiy the expression if possible **/
  def simplify = this
}

trait NullaryOpFactory [T <: NullaryOp] extends ExprFactory {
  val arity = 0
}