/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.openmole.tools.mgo.evolution.GenomeOperation
import org.openmole.tools.mgo.mappedgenome.genomedouble._
import java.util.Random

class UniformeCrossover (genomeFactory:GenomeDoubleFactory)  extends GenomeOperation[GenomeDouble] {
  
  override def operate(genomes: IndexedSeq[GenomeDouble])(implicit prng: Random): GenomeDouble = {
  
  val size:Int = genomes.head.size
  val mutationGenome:GenomeDouble = genomeFactory.apply
  val indexGenomeA = prng.nextInt(genomes.size)
  val indexGenomeB = prng.nextInt(genomes.size)
  
  if (indexGenomeB == indexGenomeA && genomes.size>1)
  {
    while ((indexGenomeB) == indexGenomeA ) {
      val indexGenomeB = prng.nextInt(genomes.size)
    } 
  }
  
  val pickedGenomeA:GenomeDouble = genomes.apply(indexGenomeA)
  val pickedGenomeB:GenomeDouble = genomes.apply(indexGenomeB)
  
  val newGenome = genomeFactory.empty()
  
  pickedGenomeA.foreach{ case(key,value) => {
        newGenome(key) = {
          if(prng.nextFloat < 0.5) pickedGenomeA.apply(key) else pickedGenomeB.apply(key)}
  }}
    return newGenome
  }

  
  

}
