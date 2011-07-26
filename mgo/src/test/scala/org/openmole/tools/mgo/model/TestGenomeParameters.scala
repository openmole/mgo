
package org.openmole.tools.mgo.model

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.genomefactory._

class TestGenomeParameters extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    /*class GenomeSLocal(v:IndexedSeq[Double], override val sigma:IndexedSeq[Double]) extends GAGenome with SigmaParameters {
      override val values = v
    }*/
    
    // Init population
    /*val gaGenomeFactory = new GenomeSigmaFactory {
      override def buildFromValues(genome: GenomeSLocal, sigmaValues: IndexedSeq [Double]): GenomeSLocal = {
         new GenomeSLocal(genome.values,sigmaValues)
        }
      }
    */
    
    // Init operator
    
    // Init engine
    //val engine = new EvolutionEngine[GenomeDouble]
    
  
  }
}
