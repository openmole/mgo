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

class UniformeCrossover (interval:IntervalSet, rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[GenomeDouble] {
  
  def this(interval:IntervalSet, rate: Double) = this(interval, _ => rate)
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random, mutationRate:Double ): GenomeDouble = {
  
  val size:Int = genomes.size
  if (size == 1){
    return genomes(0)
  }
  
  var genomeA = genomes.random
  var genomeB = genomes.random
  
  if (genomeA == genomeB && genomes.size > 1)
  {
    while ((genomeA) == genomeB ) {
      genomeA = genomes.random
    }
  }
  
  var newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)
 
  genomeA.foreach{ case(key,value) => { 
        if( rng.nextDouble  < rate(rng))
        newGenome.update(key,genomeA.apply(key))
       else
         newGenome.update(key, genomeB.apply(key))
       }
  }
 return newGenome
  }

}
