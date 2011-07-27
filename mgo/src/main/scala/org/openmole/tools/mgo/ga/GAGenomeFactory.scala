/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.genomefactory._

import java.util.Random

//trait GAGenomeFactory  [G <: GAGenome] extends GenomeFactory [G] {
//  def buildGenome (values: IndexedSeq [Double]) : G
 abstract class GAGenomeFactory  [G <: GAGenome] 
   extends GenomeFactory [G] 
   with FromValuesFactory [G]
   with FromWrappedValuesFactory [G]{
  def buildGenome (v: IndexedSeq [Double]) : G 
  def buildRandomGenome (implicit aprng:Random) : G
}
