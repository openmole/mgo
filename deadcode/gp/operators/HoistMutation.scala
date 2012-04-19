/*
 * Creates a new o\ufb00spring individual which is copy of a randomly chosen 
 * subtree of the parent.
 */

package fr.iscpif.mgo.gp.operators

import fr.iscpif.mgo._
import gp._
import ExprGenerator._

object HoistMutation extends Operator {
  val factory = null
  override def apply (genomes : IndexedSeq [Expr])
    (implicit aprng : java.util.Random) : Expr =
    chooseRandomSubtree (genomes (aprng.nextInt (genomes.size)))
}