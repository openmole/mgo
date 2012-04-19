/*

package fr.iscpif.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import fr.iscpif.mgo._
import fr.iscpif.mgo._
import fr.iscpif.mgo.operators.crossover._
import fr.iscpif.mgo.operators.mutation._
import fr.iscpif.mgo._
import fr.iscpif.mgo.algorithm._
import scala.math._
import fr.iscpif.mgo.tools.Scaling._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class SteadyGenomeParametersAckleySpec extends FlatSpec with ShouldMatchers{

  "SteadyGenomeParametersAckleySpec " should "create and initialize with good values" in {
    
    val factory = new GAGenomeWithSigmaFactory(2)
    
    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
    def evaluator(inGenome: GAGenomeWithSigma) = {
      // Nombre de dimensions de la fonction = nombre de gene dans le genome
      val genomeSize:Double = inGenome.values.size
        
      //val max:Double = 1 
      //val min:Double = 0
      val max = 32 
      val min = -32
       
      //println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)        
        
      val a = inGenome.values.map{x => x.scale(min, max)}.map{x => pow(x,2.)}.sum //sum(x(i)^2)
      val b = inGenome.values.map{x=> x.scale(min, max)}.map{x => cos(2.*Pi*x)}.sum //sum(cos(2*Pi*x(i)
      val exp1 = exp( (-0.2) * sqrt((1./genomeSize.toDouble)*a))
      val exp2 = exp((1./genomeSize.toDouble)*b) 
      val fx = 20.+ math.E - (20. * exp1) - exp2
  
      new GAFitness {
        val values = IndexedSeq(fx)
      }
      
    }
    
      
    implicit val aprng = new Random
    
    // Init random population
    val genomes = (0 until 100).map{_ => factory.random}
    
    // Init algorithms NSGA2 avec les deux types d'operateurs, select etant dans NSGA2
    val evolutionEngine = NSGAII.sigma(0.9)
      
    // First turn, evaluate and construct init population of individual
    val individus = genomes.map{g => Individual(g, evaluator)}
    val archive = evolutionEngine(individus, factory, evaluator, 1)
    println(archive.map{i => i.fitness.toString})
  }
}
*/
