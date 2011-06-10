/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import IntervalSet._

class RandomMutation(interval: IntervalSet, rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[GenomeDouble] {

  def this(interval:IntervalSet, rate: Double) = this(interval, _ => rate)
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = {
  
  val size:Int = genomes.size
  val mutationGenome:GenomeDouble = IntervalSet.intervallDecorator.generate(interval)(rng)
  
  val mutationRate = rate(rng)
  
  val pickedGenome:GenomeDouble = genomes.random
   
  val newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)

    pickedGenome.foreach{ case(key,value) => {
        newGenome.update(key, {
          if(rng.nextDouble < mutationRate) 
            pickedGenome.apply(key)
           else
            mutationGenome.apply(key)
          }
        )
      }
    }

    return newGenome
}

}
