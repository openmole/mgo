/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._

trait DiversityMetric { 
  def diversity(indiv :IndexedSeq [Individual[_]]): IndexedSeq[Diversity]
}
