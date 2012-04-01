/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.selection

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.GAGenome
import fr.iscpif.mgo.ga.domination.Dominant

trait Rank {
  def apply(individuals: IndexedSeq [Individual[GAGenome, GAFitness]], dominanceType: Dominant): IndexedSeq[Ranking]
}
