/**
 * Replaces a randomly chosen subtree with a randomly created terminal
 */

package fr.iscpif.mgo.gp.operators

import fr.iscpif.mgo._
import gp._
import ExprGenerator._

class ShrinkMutation (terms : IndexedSeq [ExprFactory]) 
  extends Operator {
  val factory = null
  override def apply (genomes : IndexedSeq [Expr])
    (implicit aprng : java.util.Random) : Expr = {
    val rpl = terms (aprng.nextInt (terms.size))
    replaceRandomSubtreeWith (genomes (aprng.nextInt (genomes.size)), rpl ())
  }
}