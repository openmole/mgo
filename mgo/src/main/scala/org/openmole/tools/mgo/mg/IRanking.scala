/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg

trait IRanking {
  var rank:Int = 0
  var dominate:IndexedSeq[Int] = IndexedSeq.empty
}
