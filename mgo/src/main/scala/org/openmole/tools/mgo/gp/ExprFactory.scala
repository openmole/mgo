/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

import java.util.Random
import org.openmole.tools.mgo._

trait ExprFactory extends GenomeFactory [Expr] {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr
  def arity : Int
}
