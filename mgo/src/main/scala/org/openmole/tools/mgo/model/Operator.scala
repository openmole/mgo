/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

import java.util.Random


trait Operator[T <: Genome, F <: GenomeFactory[T]] {  
  def operate(genomes: Population[T])(implicit factory: F, aprng: Random): T 
}
