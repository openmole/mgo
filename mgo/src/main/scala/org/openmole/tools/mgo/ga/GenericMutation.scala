/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.tools.Random._


import java.util.Random

/*abstract class GenericMutation(interval: IntervalSet,rate: Random => Double = rng => rng.nextFloat) extends Operator[GenomeDouble]{
  
  def this(interval:IntervalSet, rate: Double) = this(interval,  _ => rate)
  
  override def operate (genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = 
  {
    val pickedGenome:GenomeDouble = genomes.random
    val newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)
    val mapOfBounds = IntervalSet.intervallDecorator.generateBounds(interval)(rng)
    val mutationRate = rate(rng)
    
    pickedGenome.foreach{ case(key,value) => {
          newGenome.update(key,
                           if(rng.nextDouble < mutationRate) 
                             pickedGenome.apply(key)
                           else
                             operateMutation( pickedGenome, key,mapOfBounds.apply(key)._1,mapOfBounds.apply(key)._2))
                           }
                         }
    return newGenome    
  }
  
  def clamp( value:Double, min:Double, max:Double):Double={
    if (value >= max)
        return max
    if (value <= min)
        return min
    return value
  }
               
  def operateMutation(genome:GenomeDouble,key:String, min:Double,max:Double)(implicit rng: Random):Double
                          
}*/ 
