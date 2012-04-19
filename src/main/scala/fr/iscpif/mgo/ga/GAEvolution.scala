/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga


import fr.iscpif.mgo._

trait GAEvolution extends Evolution { self =>
  type G <: GAGenome
  type F <: GAGenomeFactory[G]
}
