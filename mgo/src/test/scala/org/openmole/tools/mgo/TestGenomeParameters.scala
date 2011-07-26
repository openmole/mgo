
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.tools.mgo._

import java.util.Random



@RunWith(classOf[JUnitRunner])
class TestGenomeParameters extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
   /* class GenomeSLocal(v:IndexedSeq[Double], override val sigma:IndexedSeq[Double]) extends GAGenome with SigmaParameters {
      override val values = v
    }
    
    class GenomeSLocalSigmaFactory extends GenomeSigmaFactory {
      override def buildFromValues(genome: GenomeSLocal, sigmaValues: IndexedSeq [Double]): GenomeSLocal = {
         new GenomeSLocal(genome.values,sigmaValues)
        }
       override def buildRandomGenome (implicit aprng:Random) : GAGenome
       {
         
       }
    }
    
    // Init population
    val gaGenomeFactory = new GenomeSLocalSigmaFactory()
    
    
    // Init operator
    
    // Init engine
    //val engine = new EvolutionEngine[GenomeDouble]
    */
  
  }
}
