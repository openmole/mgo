/*
 * Replaces a randomly selected subtree with another randomly created subtree
 */

package org.openmole.tools.mgo.gp.operators

import org.openmole.tools.mgo._
import gp._
import ExprGenerator._

class SubtreeMutation (terms : IndexedSeq [ExprFactory], funs : IndexedSeq [ExprFactory]) 
  extends Mutation [Expr, ExprFactory] {
  val factory = null
  override def operate (genomes : IndexedSeq [Expr]) 
    (implicit aprng : java.util.Random) : Expr = {
    val e = genomes (aprng.nextInt (1))
    val i = aprng.nextInt (e.size)
    e.replaceSubtreeWith(i, genExpr(funs, terms, e.depth - e.getDepth (i), "full"))
  }
}