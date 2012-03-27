/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import java.util.Random

trait GAGenomeFactory [G <: GAGenome] {
  def apply (v: IndexedSeq [Double]) : G 
  def updatedValues(g: G, values: IndexedSeq [Double]): G
  def random (implicit aprng: Random): G
}
