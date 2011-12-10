/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

abstract class Operator[G <: AbstractGenome, F <: GenomeFactory[G], R] {
  implicit val factory: F
  def operate (genomes: IndexedSeq[G]) (implicit aprng : Random):R
}
