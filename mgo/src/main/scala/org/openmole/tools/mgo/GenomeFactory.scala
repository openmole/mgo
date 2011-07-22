/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

abstract class GenomeFactory[G <: AbstractGenome](implicit aprng:Random) {
  def buildGenome(values: Array[Double]): G
  def buildRandomGenome(aprng:Random):G
}