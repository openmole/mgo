/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga.GAEvolution

trait GenomeCrowdingDistance extends DiversityMetric { this: GAEvolution =>

  def diversity(evaluated: IndexedSeq[Individual[G]]): IndexedSeq[Double] =   
    CrowdingDistance(evaluated.map{_.genome.values})
    
}
