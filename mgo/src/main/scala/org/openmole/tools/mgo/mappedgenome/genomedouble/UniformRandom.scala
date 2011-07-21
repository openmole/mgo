/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

import org.openmole.tools.mgo.model.Genome

// On passe la fonction à appliquer (operateMutation) avec sigma fixe 
trait UniformRandom[T <: Genome] {
  def operate(origin:Genome,destination:Genome)(implicit rng: Random):Double={
    
  }
}
