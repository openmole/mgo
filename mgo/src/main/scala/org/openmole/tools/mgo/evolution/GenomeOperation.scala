/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.evolution

import java.util.Random

trait GenomeOperation[T] {  
  def operate(genomes: IndexedSeq[T])(implicit aprng: Random): T 
}
