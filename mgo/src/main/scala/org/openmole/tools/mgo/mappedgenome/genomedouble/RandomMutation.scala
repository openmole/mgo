/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import java.util.Random

class RandomMutation(genomeFactory:GenomeDoubleFactory)  extends GenomeOperation[GenomeDouble] {

  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit prng: Random): GenomeDouble = {
  
  val size:Int = genomes.head.size
  val mutationGenome:GenomeDouble = genomeFactory.apply
  
  val mutationRate = prng.nextFloat
  val pickedGenome:GenomeDouble = genomes.apply(prng.nextInt(genomes.size))
  
  val newGenome = genomeFactory.empty()
  
    pickedGenome.foreach{ x => {
        newGenome(x._1) = {
          if(prng.nextFloat < mutationRate) pickedGenome.apply(x._1) else mutationGenome.apply(x._1)}
        
      }}
    
    return newGenome
}

}
