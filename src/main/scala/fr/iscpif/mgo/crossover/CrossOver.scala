/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import java.util.Random

trait CrossOver { self: Evolution =>
  def crossover(g1: G, g2: G)(implicit aprng : Random, factory: Factory[G]): IndexedSeq[G]
}