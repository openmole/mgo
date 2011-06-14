/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp

class Individual [T] (val genome : T, val fit : Double) {
  override def toString = "(" + genome.toString + "," + fit.toString + ")"
}
