/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import org.apache.commons.math.util.FastMath._
import org.openmole.tools.mgo.tools.Random._
import java.util.Random

class SoftMutation (interval: IntervalSet,rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[GenomeDouble] {

  def this(interval:IntervalSet, rate: Double) = this(interval,  _ => rate)
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = {

    val size:Int = genomes.size
    val pickedGenome:GenomeDouble = genomes.random
    val mapOfBounds = IntervalSet.intervallDecorator.generateBounds(interval)
    val newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)
    val mutationRate = rate(rng)
    
    pickedGenome.foreach{ case(key,value) => {
        newGenome.update(key, {
          if (rng.nextDouble < mutationRate)
            pickedGenome.apply(key)
          else
          { 
            0.0
            //gaussianMutation(value,mean,sigma,mapOfBounds.apply(key)._1,mapOfBounds.apply(key)._2)
          }
        }
       )
     }
  }
  
  return newGenome
  
  }
  
  def gaussianMutation(value:Double,sigma:Double,mean:Double,min:Double,max:Double)(implicit rng: Random):Double ={

    //on fait un ajustement fonction de sigma et mean
    //http://c-faq.com/lib/gaussian.html
    //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
    val gRnd = rng.nextGaussian()
    val gRndAffine =  ( gRnd  * sigma ) + mean
    
    return clamp(gRndAffine,min,max)
  }
  
  def clamp( value:Double, min:Double, max:Double):Double={
    if (value >= max)
        return max
    if (value <= min)
        return min
    return value
}
  
  }
