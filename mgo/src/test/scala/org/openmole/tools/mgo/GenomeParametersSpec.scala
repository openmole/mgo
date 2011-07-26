
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.genomefactory._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.tools.mgo._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersSpec extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    /*class GenomeSLocal(v:IndexedSeq[Double], override val sigma:IndexedSeq[Double]) extends GAGenome with SigmaParameters {
=======
   /* class GenomeSLocal(v:IndexedSeq[Double], override val sigma:IndexedSeq[Double]) extends GAGenome with SigmaParameters {
>>>>>>> 2486411d1205968332476922b571cc5392a1105c
      override val values = v
    }*/
    
<<<<<<< HEAD
    // Init population
    /*val gaGenomeFactory = new GenomeSigmaFactory {
      override def buildFromValues(genome: GenomeSLocal, sigmaValues: IndexedSeq [Double]): GenomeSLocal = {
         new GenomeSLocal(genome.values,sigmaValues)
        }
      }
    */
=======
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
    
>>>>>>> 2486411d1205968332476922b571cc5392a1105c
    
    // Init operator
    
    // Init engine
    //val engine = new EvolutionEngine[GenomeDouble]
    */
  
  }
}
