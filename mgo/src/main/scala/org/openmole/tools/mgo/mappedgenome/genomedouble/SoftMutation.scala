/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import org.apache.commons.math.util.FastMath._
import org.apache.commons.math.random.GaussianRandomGenerator
import org.openmole.tools.mgo.tools.Random._
import java.util.Random

class SoftMutation (interval: IntervalSet,sigma:Double, mean:Double,rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[GenomeDouble] {

  def this(interval:IntervalSet,sigma:Double, mean:Double, rate: Double) = this(interval, sigma, mean, _ => rate)
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = {

  val size:Int = genomes.size
  
  //FIXME : A remplacer par genomes.random a cause du decorateur de Romain
  val pickedGenome:GenomeDouble = genomes.random
  
    val gRnd = rng.nextGaussian()
    
    //on fait un ajustement fonction de sigma et mean
    //http://c-faq.com/lib/gaussian.html
    //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
    val gRndAffine =  ( gRnd  * sigma ) + mean
    
    
    val mutationRate = rate(rng)
    
    val newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)
    
    pickedGenome.foreach{ case(key,value) => {
        newGenome.update(key, {
          if (rng.nextDouble < mutationRate)
            pickedGenome.apply(key)
          else
            
            // FIXME : tester que le nombre une fois ajouté ne va pas dépasser les bornes min max de notre génome
            pickedGenome.apply(key) + gRndAffine 
        }
       )
     }
  }
  
  return newGenome
  
  }
  
  }
