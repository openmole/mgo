/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import java.util.Random

trait GenomeFactory[+G <: Genome] {
  def apply (v: IndexedSeq [Double]) : G 
  def random (implicit aprng: Random): G
}