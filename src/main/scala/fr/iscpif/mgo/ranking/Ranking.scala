/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._

trait Ranking { this: Evolution =>
  def rank(individuals: IndexedSeq [Individual[_, FIT]]): IndexedSeq[Rank]
}
