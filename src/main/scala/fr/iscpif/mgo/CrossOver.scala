/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import java.util.Random

trait CrossOver [G, F] {
  def apply(g1: G, g2: G, factory: F) (implicit aprng : Random): IndexedSeq[G]
}