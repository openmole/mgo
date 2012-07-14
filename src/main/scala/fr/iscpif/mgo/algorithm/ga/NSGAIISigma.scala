/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.algorithm.ga

import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.algorithm._
import java.util.Random
import fr.iscpif.mgo.crossover._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.selection._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo._
import fr.iscpif.mgo.termination._

trait NSGAIISigma extends NSGAII 
                     with SigmaGAEvolution {

  type G = GAGenomeWithSigma  
      
  val factory = GAGenomeWithSigma.factory(2)
  
}