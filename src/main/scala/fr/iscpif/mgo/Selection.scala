/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import java.util.Random

// TODO : on ne peut pas etendre de operator, car on traite des individu ici et non pas des genomes :(
// + pas generique car on utilise IndividualMG With With ... et il existe aussi Individual tout court ...
// le operate est pas du tout generique
trait Selection[I <: Individual[_, _]] {
  def apply(individuals: IndexedSeq[I]) (implicit aprng : Random): I 
}

