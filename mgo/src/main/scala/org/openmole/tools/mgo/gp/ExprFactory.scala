/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import java.util.Random
import org.openmole.tools.mgo._

trait ExprFactory {
  def apply (e : Expr*) : Expr
  //def buildGenome (l : List [Expr]) (implicit aprng : Random) : T
  val arity : Int
}

trait TermFactory extends ExprFactory {
  def apply (e : Expr*) : NullaryOp = null
  val arity = 0
}