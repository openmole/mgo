/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.gp.operators

import fr.iscpif.mgo.gp.Expr

trait Operator {
  def apply (genomes : IndexedSeq [Expr])(implicit aprng : java.util.Random): Expr
}
