/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

abstract class GenomeFactory[T <: Genome] {
  def buildGenome(values: Array[Double]): T
}