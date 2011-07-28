/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import java.util.Random

object ExprGenerator {
  //Generate a random expression
  def expr (vars : IndexedSeq [ExprFactory], depth : Int, meth : String) 
           (implicit aprng : Random) = 
     genExpr (IndexedSeq (Sum, Prod, Div, Cos), Num +: vars, depth, meth)
 
  //Generate a random terminal
  def term (vars : Array [Expr], min : Double, max : Double) 
           (implicit aprng : Random) : Expr = {
    (vars :+ (Num ((aprng.nextDouble * (max - min)) + min))) apply (aprng.nextInt(vars.size+1))
  }
  
  def genExpr (funs : IndexedSeq [ExprFactory], terms : IndexedSeq [ExprFactory],
               depth : Int, method : String) (implicit aprng : Random) : Expr = {
    if (depth == 0 || (method == "grow" &&
                       aprng.nextFloat < (terms.size / (terms.size + funs.size)))) 
      terms (aprng.nextInt (terms.size)) (Nil:_*)
    else {
      val f = funs (aprng.nextInt (funs.size))
      f apply (List.fill (f.arity) (genExpr (funs, terms, depth - 1, method)):_*)
    }
    
  }
  
  def replaceRandomSubtreeWith (from : Expr, by : Expr) 
  (implicit aprng : Random) : Expr = 
    from.replaceSubtreeWith (aprng.nextInt (from.size), by)

  def chooseRandomSubtree (t : Expr) (implicit aprng : Random) : Expr =
    t.getSubtree (aprng.nextInt (t.size))

  def chooseRandomSubtreeAtDepth (t : Expr, d : Int) (implicit aprng : Random) : Expr =
    if (d == 0) t
    else chooseRandomSubtreeAtDepth (t.subtrees (aprng.nextInt (t.arity)), d-1)
 }

