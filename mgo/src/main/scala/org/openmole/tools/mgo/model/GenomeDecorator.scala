/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

trait GenomeDecorator extends Genome {
  abstract override def wrappedValues = super.values
  abstract override def values = super.values
}
