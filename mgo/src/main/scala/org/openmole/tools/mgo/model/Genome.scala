/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

abstract class Genome (v: Array[Double]){
  def position = 0
  def wrappedValues = values
  def values: Array[Double] = v
}
