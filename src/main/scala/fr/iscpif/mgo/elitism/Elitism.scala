/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._

trait Elitism { this: Evolution =>
  def elitism(individuals: Population[G, MF]): Population[G, MF]
}
