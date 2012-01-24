/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators.crossover

import org.openmole.tools.mgo._
import ga._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import scala.math._

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

class SBXBoundedCrossover[G <: GAGenome, F <: GAGenomeFactory[G]](rate: Random => Double = _.nextDouble) extends CrossOver [G, F] {
    
  def this(rate: Double) = this( _ => rate)
  
  /* VERSION EN DEBUT DE FONCTIONNEL */
  def crossOver (genomes : IndexedSeq [G], factory: F) (implicit aprng : Random) = {
    val g1 = genomes.random
    val g2 = genomes.random
    val crossoverRate = rate(aprng)
    val EPS =  1.0e-14
    val numberOfVariables = g1.wrappedValues.size
    //FIXME : aucune idée de la valeur qu'il faudrait mettre ici, si on raisonne entre 0 et 1, pour le moment on laisse la valeur a 20
    val distributionIndex = 20
  
    val variableToMutate = (0 until g1.wrappedValues.size).map{x => !(aprng.nextDouble < 0.5)}
      
    //crossover probability
    val offspring = {
      
      if (aprng.nextDouble <= crossoverRate) {      
        (variableToMutate zip (g1.wrappedValues zip g2.wrappedValues)) map {
          case (b, (g1e, g2e)) =>
            if(b) {
              if (abs(g1e - g2e) > EPS){
             
                val y1 = min(g1e, g2e)
                val y2 = max(g2e, g1e)
              
                var yL = 0.0 //g1e.getLowerBound
                var yu = 1.0 //g1e.getUpperBound  
                var rand = aprng.nextDouble   // ui
              
                var beta1 = 1.0 + (2.0 * (y1 - yL)/(y2 - y1))
                var alpha1 = 2.0 - pow(beta1,-(distributionIndex+1.0))
                var betaq1 = computebetaQ(alpha1,distributionIndex,rand)
              
                //calcul offspring 1 en utilisant betaq1, correspond au β barre
                var c1 = 0.5 * ((y1 + y2) - betaq1 * (y2 - y1)) 
              
                // -----------------------------------------------
              
                var beta2 = 1.0 + (2.0 * (yu - y2) / (y2 - y1))
                var alpha2 = 2.0 - pow(beta2, -(distributionIndex + 1.0))
              
                var betaq2 = computebetaQ(alpha2,distributionIndex,rand)
              
                //calcul offspring2 en utilisant betaq2
                var c2 = 0.5 * ((y1 + y2) + betaq2 * (y2 - y1))
              
                if (c1 < yL) c1 = yL
                if (c1 > yu) c1 = yu
              
                if (c2 < yL) c2 = yL
                if (c2 > yu) c2 = yu   
              
                if (aprng.nextDouble <= 0.5) {
                  (c2,c1)
                } else {
                  (c1, c2) 
                }
              
              }else{
                (g1e, g2e)
              }
            
            }else{
              (g2e, g1e)
            }
        }
      
      }else{
        // not so good here ...
        (g1.wrappedValues zip g2.wrappedValues)
      }
    }
    (factory.buildGenome(offspring.map{_._1}),  factory.buildGenome(offspring.map{_._2}))
  }

  def computebetaQ(alpha:Double,  distributionIndex:Double,  rand:Double):Double = { 
    if (rand <= (1.0/alpha)){
      pow ((rand * alpha),(1.0 / (distributionIndex + 1.0)))
    } else {
      pow ((1.0 / (2.0 - rand * alpha)),(1.0 / (distributionIndex + 1.0)))
    } 
  }

}

