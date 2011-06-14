/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import java.util.Random

object ExprGenerator {
  //Generate a random expression
  def expr (vars : Array [Expr], depth : Int, meth : String) (implicit rng : Random) = 
     genExpr (Array (Sum, Prod, Div, Sin, Cos), vars, depth, meth)
  
  //Generate a random terminal
  def term (vars : Array [Expr]) (implicit rng : Random) : Expr = {
    (vars :+ (Num (rng.nextDouble))) apply (rng.nextInt(vars.size+1))
  }
  
  def genExpr (funs : Array [ExprFactory], vars : Array [Expr], depth : Int,
           method : String) (implicit rng : Random) : Expr = {
    if (depth == 0 || (method == "grow" &&
                       rng.nextFloat < (vars.size / (vars.size + funs.size)))) 
      term (vars)
    else {
      val f = funs (rng.nextInt (funs.size))
      f (Seq.fill (f.arity) (genExpr (funs, vars, depth - 1, method)):_*)
    }
  }
  
  def replaceRandomSubtreeWith (from : Expr, by : Expr) 
  (implicit rng : Random) : Expr = 
    from.replaceSubtreeWith (rng.nextInt (from.size), by)

  def chooseRandomSubtree (t : Expr) (implicit rng : Random) : Expr =
    t.getSubtree (rng.nextInt (t.size))

  def chooseRandomSubtreeAtDepth (t : Expr, d : Int) (implicit rng : Random) : Expr =
    if (d == 0) t
    else chooseRandomSubtreeAtDepth (t.subtrees (rng.nextInt (t.arity)), d-1)
 }

