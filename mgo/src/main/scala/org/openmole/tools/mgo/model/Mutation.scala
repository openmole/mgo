/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

class Mutation[T <: Genome, F <: GenomeFactory[T]] extends Operator [T, F]{
  def operate()
}
