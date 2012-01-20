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
   */

  /* CODE JMETAL ORIGINAL */
  
  /*
   * public Solution[] doCrossover(double probability, 
   Solution parent1, 
   Solution parent2) throws JMException {
    
   Solution [] offSpring = new Solution[2];

   offSpring[0] = new Solution(parent1);
   offSpring[1] = new Solution(parent2);
                    
   int i;
   double rand;
   double y1, y2, yL, yu;
   double c1, c2;
   double alpha, beta, betaq;
   double valueX1,valueX2;
   XReal x1 = new XReal(parent1) ;		
   XReal x2 = new XReal(parent2) ;		
   XReal offs1 = new XReal(offSpring[0]) ;
   XReal offs2 = new XReal(offSpring[1]) ;
		
   int numberOfVariables = x1.getNumberOfDecisionVariables() ;

   if (PseudoRandom.randDouble() <= probability){
    
   for (i=0; i<numberOfVariables; i++){
      
   valueX1 = x1.getValue(i);
   valueX2 = x2.getValue(i);
        
   if (PseudoRandom.randDouble()<=0.5 ) {
   if (java.lang.Math.abs(valueX1- valueX2) > EPS){
            
   if (valueX1 < valueX2){
   y1 = valueX1;
   y2 = valueX2;
   } else {
   y1 = valueX2;
   y2 = valueX1;
   } // if                       
            
   yL = x1.getLowerBound(i) ;
   yu = x1.getUpperBound(i) ;
   rand = PseudoRandom.randDouble();
   beta = 1.0 + (2.0*(y1-yL)/(y2-y1));
   alpha = 2.0 - java.lang.Math.pow(beta,-(distributionIndex_+1.0));
            
   if (rand <= (1.0/alpha)){
   betaq = java.lang.Math.pow ((rand*alpha),(1.0/(distributionIndex_+1.0)));
   } else {
   betaq = java.lang.Math.pow ((1.0/(2.0 - rand*alpha)),(1.0/(_+1.0)));
   } // if
            
   c1 = 0.5*((y1+y2)-betaq*(y2-y1));
   beta = 1.0 + (2.0*(yu-y2)/(y2-y1));
   alpha = 2.0 - java.lang.Math.pow(beta,-(distributionIndex_+1.0));
            
   if (rand <= (1.0/alpha)){
   betaq = java.lang.Math.pow ((rand*alpha),(1.0/(distributionIndex_+1.0)));
   } else {
   betaq = java.lang.Math.pow ((1.0/(2.0 - rand*alpha)),(1.0/(distributionIndex_+1.0)));
   } // if
              
   c2 = 0.5*((y1+y2)+betaq*(y2-y1));
            
   if (c1<yL)
   c1=yL;
            
   if (c2<yL)
   c2=yL;
            
   if (c1>yu)
   c1=yu;
            
   if (c2>yu)
   c2=yu;                        
              
   if (PseudoRandom.randDouble()<=0.5) {
   offs1.setValue(i, c2) ;
   offs2.setValue(i, c1) ;
   } else {
   offs1.setValue(i, c1) ;
   offs2.setValue(i, c2) ;
   } // if
   } else {
   offs1.setValue(i,valueX1) ;
   offs2.setValue(i, valueX2) ;
   } // if
   } else {
   offs1.setValue(i, valueX2) ;
   offs2.setValue(i, valueX1) ;
   } // if
   } // if
   } // if
                                    
   return offSpring;                                                                                      
   } // doCrossover
  
   */
  
class SBXBoundedCrossover [G <: GAGenome, F <: GAGenomeFactory [G]] (rate: Random => Double = rng => rng.nextFloat)(implicit val factory : F)
extends CrossOver [G, F]{
    
  def this( rate: Double, factory : F) = this( _ => rate)(factory)
  
  /* VERSION EN DEBUT DE FONCTIONNEL */
  def operate (genomes : IndexedSeq [G]) (implicit aprng : Random) : IndexedSeq[G] = {
    val g1 = genomes.random
    val g2 = genomes.random
    val crossoverRate = rate(aprng)
    val EPS =  1.0e-14
    val numberOfVariables = g1.wrappedValues.size
    val distributionIndex = 2
    
    //crossover probability
    if (aprng.nextDouble < crossoverRate) {
      
      val variableToMutate = (0 until g1.wrappedValues.size).map{x => !(aprng.nextDouble < 0.5)}
      val offspringValues = (variableToMutate zip (g1.wrappedValues zip g2.wrappedValues)) map {
        case (b, (g1e, g2e)) =>
          if(b) {
            if (abs(g1e - g2e) > EPS){
             
              val y1 = min(g1e, g2e)
              val y2 = max(g2e, g1e)
              
              var yL = 0.0 //g1e.getLowerBound
              var yu = 1.0 //g1e.getUpperBound  
              var rand = aprng.nextDouble   // ui
              
              //
              //Dans jmetal il utilise et ecrase toujours les meme variable beta / alpha / betaq,
              // mais les valeurs change, pour alpha et betaq .. donc j'ai renommé les variables pour mieux suivre le cheminement 
              
              // Bizarre cette valeur de beta1 / beta2 , à comparer avec le beta modifié decrit dans equation de l'annexe A du papier de DEB
              
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
      
    }
    
    offspringValues
  }
    
  // False on echange, true on maintient
  //val rngValue:IndexedSeq[Boolean] = (0 until g1.wrappedValues.size).map{x => if (aprng.nextDouble < crossoverRate) false else true }
  //val offspringValues = (g1.wrappedValues,g2.wrappedValues,rngValue).zipped.map((x, y, z) => if (z) IndexedSeq(x, y) else IndexedSeq(y, x)).transpose
    
  //return offspringValues.map{factory.buildGenome (_)}.toIndexedSeq


  
  def computebetaQ(alpha:Double,  distributionIndex:Double,  rand:Double):Double = { 
    if (rand <= (1.0/alpha)){
      pow ((rand * alpha),(1.0 / (distributionIndex + 1.0)))
    } else {
      pow ((1.0 / (2.0 - rand * alpha)),(1.0 / (distributionIndex + 1.0)))
    } 
  }

}

