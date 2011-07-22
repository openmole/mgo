/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

trait Operator[G <: AbstractGenome, F <: GenomeFactory[G]] {  
  def operate(genomes: Population[G]) (implicit factory: F, aprng: Random): G 
}
