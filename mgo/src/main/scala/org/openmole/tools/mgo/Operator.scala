/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

trait Operator[G, F] {
  def apply(genomes: IndexedSeq[G], factory: F) (implicit aprng : Random): Iterable[G]
}
