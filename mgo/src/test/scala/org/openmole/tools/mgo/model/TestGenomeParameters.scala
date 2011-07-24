
package org.openmole.tools.mgo.model

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.tools.mgo.asrea._
import org.openmole.tools.mgo.evolution._
import org.openmole.tools.mgo.tools.Random._

import java.util.Random

import org.openmole.tools.mgo.ga._

class TestGenomeParameters extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    val sigmaGenome = new GAGenome (Array(0.,1.)) with SigmaParameters {
      override def sigma = Array(2.,3.)
    }
    
    
  
  }
}
