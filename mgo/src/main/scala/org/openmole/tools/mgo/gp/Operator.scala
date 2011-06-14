/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp
import ExprGenerator._

trait Operator [T] {
  def generateGenome (population : Iterable [Individual [T]]) 
                     (implicit rng : java.util.Random) : T
}

trait CrossOverOperator [A] extends Operator [A]

trait MutationOperator [A] extends Operator [A]

class ShrinkMutation (vars : Iterable [Expr]) extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]])
                     (implicit rng : java.util.Random) : Expr = {
    val rpl = term (vars.toArray)
    replaceRandomSubtreeWith (pop.toArray.apply (rng.nextInt (pop.size)) genome, rpl)
  }
}

object HoistMutation extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]]) 
                     (implicit rng : java.util.Random) : Expr = 
    chooseRandomSubtree (pop.toArray.apply (rng.nextInt (pop.size)) genome)
}

class SubtreeMutation (vars : Iterable [Expr], funs : Iterable [ExprFactory]) 
extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]]) 
                     (implicit rng : java.util.Random) : Expr = {
    val e   = pop.toArray.apply (rng.nextInt (1)) genome
    val i = rng.nextInt (e.size)
    e.replaceSubtreeWith(i, genExpr(funs.toArray, vars.toArray, 
                                            e.depth - e.getDepth (i), "full"))
  }
}

object CrossingOver extends CrossOverOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]]) 
                     (implicit rng : java.util.Random) : Expr = {
    val (t1, t2) = (pop.toArray.apply (rng.nextInt (pop.size)) genome, 
                    pop.toArray.apply (rng.nextInt (pop.size)) genome)
    val (deeper, shallow) = 
      if (t1.depth >= t2.depth) (t1, t2) else (t2, t1)
    val i = rng.nextInt (shallow.size)
    val depth = shallow.getDepth(i)
    val subtrees = deeper.subtreesAtDepth(depth)
    shallow.replaceSubtreeWith(i, subtrees (rng.nextInt (subtrees.size)))
  }
}
