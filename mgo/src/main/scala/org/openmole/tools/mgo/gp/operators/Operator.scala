/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp.operators

import org.openmole.tools.mgo.gp.Expr

trait Operator {
  def apply (genomes : IndexedSeq [Expr])(implicit aprng : java.util.Random): Expr
}
