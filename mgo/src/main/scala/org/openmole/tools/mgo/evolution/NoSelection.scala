/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.evolution

object NoSelection extends ISelection[Any] {
  override def accept(o: Any): Boolean = true
}
