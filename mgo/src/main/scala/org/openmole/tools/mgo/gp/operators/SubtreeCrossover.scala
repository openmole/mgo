/**
 * Taking two trees, replace a random subtree in the most shallow by a random
 * subtree at same depth in the deeper.
 */

package org.openmole.tools.mgo.gp.operators

import org.openmole.tools.mgo._ 
import gp._
import ExprGenerator._

object SubtreeCrossOver extends Operator {
  val factory = null
  override def apply (genomes : IndexedSeq [Expr]) 
    (implicit aprng : java.util.Random) : Expr = {
    val (t1, t2) = (genomes (aprng.nextInt (genomes.size)), 
                    genomes (aprng.nextInt (genomes.size)))
    
    val (deeper, shallow) = 
      if (t1.depth >= t2.depth) (t1, t2) else (t2, t1)
    
    val i = aprng.nextInt (shallow.size)
    val depth = shallow.getDepth(i)
    val subtrees = deeper.subtreesAtDepth (depth)
    
    shallow.replaceSubtreeWith(i, subtrees (aprng.nextInt (subtrees.size)))
  }
}