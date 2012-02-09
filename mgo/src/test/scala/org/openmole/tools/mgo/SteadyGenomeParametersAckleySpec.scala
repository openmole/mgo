
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.operators.crossover._
import org.openmole.tools.mgo.ga.operators.mutation._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.algorithm.NSGAII
import scala.math._
import org.openmole.tools.mgo.tools.Scaling._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class SteadyGenomeParametersAckleySpec extends FlatSpec with ShouldMatchers{

  "SteadyGenomeParametersAckleySpec " should "create and initialize with good values" in {
    
    trait GenomeAckley extends GAGenome with SigmaParameters {
      def wrappedValues = values ++ sigma
    }
    
    class GenomeAckleyFactory extends GAGenomeFactory[GenomeAckley] with GASigmaParametersFactory [GenomeAckley] {
      override def buildGenome(v : IndexedSeq[Double]) = 
        new GenomeAckley {
          val values = v.slice(0, 2)
          val sigma = v.slice(2, 4)
        } 
      
      override def buildFromValues(genome: GenomeAckley, _values: IndexedSeq [Double]) = 
        new GenomeAckley {
          val values = _values
          val sigma = genome.sigma
        }

      override def buildFromSigma(genome: GenomeAckley, _sigma: IndexedSeq [Double]) = 
        new GenomeAckley {
          val values = genome.values
          val sigma = _sigma
        }
      
      def buildRandomGenome (implicit aprng : Random):GenomeAckley = 
        new GenomeAckley {
          val values = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
          val sigma = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
        } 

    }
    
    val factory = new GenomeAckleyFactory
    
    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
    def evaluator(inGenome: GenomeAckley) = {
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
  
      new Individual[GenomeAckley, GAFitness] {
        def genome = inGenome
        def fitness = new GAFitness {
          val fitness = IndexedSeq(fx)
        }
      }
      
    }
    
    initTest
   
    def initTest = {
      
      implicit val aprng = new Random
      implicit val function: Random => Double = arpng => arpng.nextFloat
    
      // Init random population
      var genomes: IndexedSeq[GenomeAckley] = (0 until 100).map{_ => factory.buildRandomGenome}
    
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeAckley, GenomeAckleyFactory] 
      val sbxCross = new SBXBoundedCrossover[GenomeAckley, GenomeAckleyFactory](0.9)
     
      // Init algorithms NSGA2 avec les deux types d'operateurs, select etant dans NSGA2
      val evolutionEngine = new NSGAII(softMut, sbxCross)
      
      // First turn, evaluate and construct init population of individual
      var individus = evolutionEngine.select(IndexedSeq.empty, genomes.map{g => evaluator(g)}, genomes.size)._1
      var nbSimilar = 0
      
      //Generation evolve 1 by 1
      val archive = (0 to 800).foldLeft(individus){
        (acc, gen) => 
          val offspring = evolutionEngine.generate(acc,factory, 1).map{evaluator} // create breed population and evaluate to make new individuals
          val result = evolutionEngine.select(acc,offspring, acc.size) //merge archive and recent evaluated population, then sample the best individuals to make a new archive
          if (!result._2) nbSimilar += 1 else nbSimilar = 0
          result._1
      }

      println(archive.map{i => i.fitness.toString})
    }
  }
}
