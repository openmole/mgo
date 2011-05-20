/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import java.util.Random
import IntervalSet._

class UniformeCrossover (interval:IntervalSet) extends GenomeOperation[GenomeDouble] {
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = {
  
  val size:Int = genomes.size
  if (size == 1){
    return genomes(0)
  }
  var indexGenomeA = rng.nextInt(genomes.size)
  var indexGenomeB = rng.nextInt(genomes.size)
  
  if (indexGenomeB == indexGenomeA && genomes.size>1)
  {
    while ((indexGenomeB) == indexGenomeA ) {
      indexGenomeB = rng.nextInt(genomes.size)
    }
  }
  
  val pickedGenomeA:GenomeDouble = genomes.apply(indexGenomeA)
  val pickedGenomeB:GenomeDouble = genomes.apply(indexGenomeB)
  
    
  var newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)
 
  pickedGenomeA.foreach{ case(key,value) => {
        var nextF = rng.nextFloat 
        if(nextF < 0.5)
        newGenome.update(key,pickedGenomeA.apply(key))
       else
         newGenome.update(key, pickedGenomeB.apply(key))
       }
  }
 return newGenome
  }

}
