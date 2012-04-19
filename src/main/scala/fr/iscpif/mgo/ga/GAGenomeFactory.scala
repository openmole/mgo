/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo._

trait GAGenomeFactory[+G <: GAGenome] extends GenomeFactory[G] {
  def apply(g: GAGenome): G = this(g.wrappedValues)
}
