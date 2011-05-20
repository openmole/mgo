/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import java.util.Random
import IntervalSet._

class RandomMutation(interval: IntervalSet) extends GenomeOperation[GenomeDouble] {

  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit rng: Random): GenomeDouble = {
  
  val size:Int = genomes.size
  val mutationGenome:GenomeDouble = IntervalSet.intervallDecorator.generate(interval)(rng)
  
  val mutationRate = rng.nextFloat
  //FIXME : A remplacer par genomes.random a cause du decorateur de Romain
  val pickedGenome:GenomeDouble = genomes.apply(rng.nextInt(genomes.size))
   
  val newGenome = IntervalSet.intervallDecorator.generateEmpty(interval)

  
    pickedGenome.foreach{ case(key,value) => {
        newGenome.update(key, {
          if(rng.nextFloat < mutationRate) 
            pickedGenome.apply(key)
           else 
            mutationGenome.apply(key)
            
          })}
    }
    

    return newGenome
}

}
