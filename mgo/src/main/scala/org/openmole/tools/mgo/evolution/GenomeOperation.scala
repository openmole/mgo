/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.evolution

import org.openmole.tools.distrng.prng.IPRNG


trait GenomeOperation[T] {
  def rndmChoice(rng: IPRNG[_], set: T*): T = {
    set(rng.nextInt(0, set.length))
  }

  def rndmChoice(rng: IPRNG[_], t1: T, t2: T): T = {
    if(rng.nextDouble <  0.5) t1 else t2
  }
    
  def operate(genomes: IndexedSeq[T], prng: IPRNG[_]): T 
}
