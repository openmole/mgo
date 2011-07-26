/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import java.util.Random

//trait GAGenomeFactory  [G <: GAGenome] extends GenomeFactory [G] {
//  def buildGenome (values: IndexedSeq [Double]) : G
 abstract class GAGenomeFactory  [G <: GAGenome] extends GenomeFactory [G] {
  def buildGenome (v: IndexedSeq [Double]) : G =  new GAGenome {override val values = v}.asInstanceOf [G]
  def buildRandomGenome (implicit aprng:Random) : G
}
