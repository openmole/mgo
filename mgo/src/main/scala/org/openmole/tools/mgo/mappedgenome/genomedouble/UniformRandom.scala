/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

import org.openmole.tools.mgo.AbstractGenome
import java.util.Random

// On passe la fonction à appliquer (operateMutation) avec sigma fixe 
trait UniformRandom [G <: AbstractGenome] {
  def operate (origin : G, destination : G) (implicit rng: Random) : Double
}
