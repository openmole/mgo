/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import java.util.Random

trait CrossOver [G, F] extends Operator [G, F] {
  def crossOver(genomes: IndexedSeq[G], factory: F) (implicit aprng : Random): (G, G)
  def apply(genomes: IndexedSeq[G], factory: F) (implicit aprng : Random) = 
    crossOver(genomes, factory) match {
      case (g1, g2) => IndexedSeq(g1, g2)
    }

}