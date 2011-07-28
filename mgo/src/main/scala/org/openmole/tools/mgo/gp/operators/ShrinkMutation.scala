/**
 * Replaces a randomly chosen subtree with a randomly created terminal
 */

package org.openmole.tools.mgo.gp.operators

import org.openmole.tools.mgo._
import gp._
import ExprGenerator._

class ShrinkMutation (terms : IndexedSeq [ExprFactory]) 
  extends Mutation [Expr, ExprFactory] {
  val factory = null
  override def operate (genomes : IndexedSeq [Expr])
    (implicit aprng : java.util.Random) : Expr = {
    val rpl = terms (aprng.nextInt (terms.size))
    replaceRandomSubtreeWith (genomes (aprng.nextInt (genomes.size)), 
                              rpl ())
  }
}