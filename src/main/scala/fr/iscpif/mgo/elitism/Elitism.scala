/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._

trait Elitism { this: Evolution =>
  def elitism(individuals: IndexedSeq[I]): IndexedSeq[I]
}
