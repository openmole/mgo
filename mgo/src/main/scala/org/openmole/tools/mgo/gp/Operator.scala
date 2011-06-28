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

/**
 * Replaces a randomly chosen subtree with a randomly created terminal
 */
class ShrinkMutation (terms : IndexedSeq [ExprFactory]) 
extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]])
                     (implicit rng : java.util.Random) : Expr = {
    val rpl = terms (rng.nextInt (terms.size))
    replaceRandomSubtreeWith (pop.toArray.apply (rng.nextInt (pop.size)) genome, 
                              rpl.build (Nil))
  }
}

/*
 * Creates a new o\ufb00spring individual which is copy of a randomly chosen 
 * subtree of the parent.
 */
object HoistMutation extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]]) 
                     (implicit rng : java.util.Random) : Expr = 
    chooseRandomSubtree (pop.toArray.apply (rng.nextInt (pop.size)) genome)
}

/*
 * Replaces a randomly selected subtree with another randomly created subtree
 */
class SubtreeMutation (terms : IndexedSeq [ExprFactory], funs : IndexedSeq [ExprFactory]) 
extends MutationOperator [Expr] {
  def generateGenome (pop : Iterable [Individual [Expr]]) 
                     (implicit rng : java.util.Random) : Expr = {
    val e = pop.toArray.apply (rng.nextInt (1)) genome
    val i = rng.nextInt (e.size)
    e.replaceSubtreeWith(i, genExpr(funs, terms, 
                                    e.depth - e.getDepth (i), "full"))
  }
}

/**
 * Taking two trees, replace a random subtree in the most shallow by a random
 * subtree at same depth in the deeper.
 */
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
