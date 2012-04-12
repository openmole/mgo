/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators.crossover

import fr.iscpif.mgo._
import ga._
import java.util.Random
import fr.iscpif.mgo.tools.Random._
import scala.math._
import fr.iscpif.mgo.tools.Math._

/**
 * SBX RGA operator with Bounded Variable modification, see APPENDIX A p30 into :
 * 
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7291&rep=rep1&type=pdf
 * 
 * @INPROCEEDINGS{Deb98anefficient,
 *   author = {Kalyanmoy Deb},
 *   title = {An Efficient Constraint Handling Method for Genetic Algorithms},
 *   booktitle = {Computer Methods in Applied Mechanics and Engineering},
 *   year = {1998},
 *   pages = {311--338}
 * }
 * 
 * Notes : Deb implementation differs from NSGA2 he proposed on this site : 
 * http://www.iitk.ac.in/kangal/codes.shtml
 *  
 */

trait AbstractSBXBoundedCrossover[G <: GAGenome, F <: GAGenomeFactory[G]] extends CrossOver [G, F] {
     
  def crossover (g1: G, g2: G, distributionIndex: Double, crossoverRate: Double, factory: F) (implicit aprng : Random) = {
    val numberOfVariables = g1.wrappedValues.size
      
    //crossover probability
    val offspring = {
      if (aprng.nextDouble <= crossoverRate) {      
        (g1.wrappedValues zip g2.wrappedValues).map {
          case (g1e, g2e) =>
            if(aprng.nextBoolean) {
              if (abs(g1e - g2e) > epsilon){
                val y1 = min(g1e, g2e)
                val y2 = max(g2e, g1e)
                
                val yL = 0.0 //g1e.getLowerBound
                val yU = 1.0 //g1e.getUpperBound  
                
                def inBound(v: Double) = if(v < yL) yL else if(v > yU) yU else v
                
                val rand = aprng.nextDouble   // ui
              
                val beta1 = 1.0 + (2.0 * (y1 - yL)/(y2 - y1))
                val alpha1 = 2.0 - pow(beta1,-(distributionIndex+1.0))
                
                val betaq1 = {                  
                  if (rand <= (1.0/alpha1)) pow ((rand * alpha1),(1.0 / (distributionIndex + 1.0)))
                  else pow ((1.0 / (2.0 - rand * alpha1)),(1.0 / (distributionIndex + 1.0)))
                }
              
                //calcul offspring 1 en utilisant betaq1, correspond au β barre
                val c1 = inBound(0.5 * ((y1 + y2) - betaq1 * (y2 - y1))) 
              
                // -----------------------------------------------
              
                val beta2 = 1.0 + (2.0 * (yU - y2) / (y2 - y1))
                val alpha2 = 2.0 - pow(beta2, -(distributionIndex + 1.0))
                
                val betaq2 = {                  
                  if (rand <= (1.0/alpha2)) pow ((rand * alpha2),(1.0 / (distributionIndex + 1.0)))
                  else pow ((1.0 / (2.0 - rand * alpha2)),(1.0 / (distributionIndex + 1.0)))
                }
              
                //calcul offspring2 en utilisant betaq2
                val c2 = inBound(0.5 * ((y1 + y2) + betaq2 * (y2 - y1)))
       
                if (aprng.nextBoolean) (c2, c1) else (c1, c2)
              } else(g1e, g2e)
            } else (g2e, g1e)
        }
      } else(g1.wrappedValues zip g2.wrappedValues)
    }
    IndexedSeq(factory(offspring.map{_._1}),  factory(offspring.map{_._2}))
  }
  
  
}

