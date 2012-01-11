/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random
import org.openmole.tools.mgo.mg._
import org.openmole.tools.mgo.mg.IndividualMG
import org.openmole.tools.mgo.model.MultiGoalLike

// TODO : on ne peut pas etendre de operator, car on traite des individu ici et non pas des genomes :(
// + pas generique car on utilise IndividualMG With With ... et il existe aussi Individual tout court ...
// le operate est pas du tout generique
trait Selection [G <: Genome,   MG <: MultiGoalLike, F <: GenomeFactory [G]] {
   def operate (individuals: IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance], numberGenerated:Int) (implicit aprng : Random): IndexedSeq[IndividualMG[G,MG] with IRanking with IDistance] 

}

