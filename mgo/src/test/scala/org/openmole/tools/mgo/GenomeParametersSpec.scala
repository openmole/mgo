
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.genomefactory._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersSpec extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    
    
    class GenomeSLocal(v:IndexedSeq[Double]) extends GAGenome with SigmaParameters {
      def this (v:IndexedSeq[Double], sigma:IndexedSeq[Double]) = this(v++ v.takeRight(sigma.size))
      override val values = v
    }
    
    class GenomeSLocalSigmaFactory extends GenomeSigmaFactory[GenomeSLocal] {
      

      
      override def buildGenome(v:IndexedSeq[Double]): GenomeSLocal = {
        new GenomeSLocal(v)
      }
      
      override def buildFromValues(genome: GenomeSLocal, sigmaValues: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values,sigmaValues)
      }
        
       def buildRandomGenome: GenomeSLocal = {
        new GenomeSLocal(IndexedSeq[Double](1,2,3), IndexedSeq[Double](1,2,3))
      }
    }
    
    
    // Init population
    


    
    // Init population
    val gaGenomeFactory = new GenomeSLocalSigmaFactory()
    
    // Init operator
    
    // Init engine
    //val engine = new EvolutionEngine[GenomeDouble]
    
  
  }
}
