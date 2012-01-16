
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.selection._
import org.openmole.tools.mgo.ga.operators._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.algorithm.NSGAII
import scala.math._
import org.openmole.tools.mgo.tools.Scaling._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersAckleySpec extends FlatSpec with ShouldMatchers{

  "GenomeParametersAckleySpec " should "create and initialize with good values" in {

    ////////////////////////////////
    // GENOME SIGMA FACTORY
    ////////////////////////////////

    
    trait GenomeAckley extends GAGenome with SigmaParameters {
      def wrappedValues = values ++ sigma
    }
    
<<<<<<< HEAD
    class GenomeSLocalFactory extends GAGenomeFactory[GenomeSLocal] with GASigmaParametersFactory [GenomeSLocal] {
=======
    implicit object GenomeAckleySigmaFactory extends GAGenomeSigmaFactory [GenomeAckley] {
>>>>>>> b3847c30c8c7540f0e2b71a2b5cee5af241fc307
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
      
      def buildRandomGenome (implicit aprng : Random) = 
        new GenomeAckley {
          val values = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
          val sigma = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
        } 

    }
    
    val factory = new GenomeSLocalFactory
    
    /*object IndividualMGFactoryDistanceRank extends IndividualMGFactory[MultiGoal,GenomeSLocal]{
     override def operate
     }*/
    
    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
<<<<<<< HEAD
    def evaluator(inGenome: GenomeSLocal) = {
      // Nombre de dimensions de la fonction = nombre de gene dans le genome
      val genomeSize:Double = inGenome.values.size
=======
    implicit object IndividuFactory 
    extends IndividualMGFactory[MultiGoal,GenomeAckley,IndividualMG[GenomeAckley,MultiGoal] with IRanking with IDistance]
    {
        
      override def operate(genome:GenomeAckley):IndividualMG[GenomeAckley,MultiGoal] with IRanking with IDistance = {
    
        // Nombre de dimensions de la fonction = nombre de gene dans le genome
        val genomeSize:Double = genome.values.size
>>>>>>> b3847c30c8c7540f0e2b71a2b5cee5af241fc307
        
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
  
      new Individual[GenomeSLocal, GAFitness] {
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
    
<<<<<<< HEAD
      // Init random population
      var genomes: IndexedSeq[GenomeSLocal] = (0 until 100).map{_ => factory.buildRandomGenome}
    
    
      //val randomMut = new RandomWrappedValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 0.1d)(GenomeSLocalSigmaFactory)
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeSLocal, GenomeSLocalFactory] 
      val randomCross = new RandomWrappedValuesCrossOver[GenomeSLocal, GenomeSLocalFactory](0.5)
     
      // Init algorithms NSGA2 avec les trois types d'operateurs
      val evolutionEngine = new NSGAII(softMut, randomCross, evaluator)
       
      // Premier tour, obligatoire pour l'initiatlisation des premier individu
      var individus = evolutionEngine.select(genomes.map{g => evaluator(g)}, genomes.size)
      
      //Generation evolve
      val archive = (0 to 800).foldLeft(individus){
        (acc, gen) => 
          val result = evolutionEngine(acc, factory)
          println("generation" + gen)
          result
=======
      // Init random population, equal to 200 genomes here
      var genomes:IndexedSeq[GenomeAckley] = (0 until 200).map{_ => GenomeAckleySigmaFactory.buildRandomGenome}
    
      // Init de l'archive population, vide au premier tour
      var archive = new PopulationMG[GenomeAckley,IndividualMG[GenomeAckley,MultiGoal] with IRanking with IDistance](IndexedSeq.empty)
    
      //val randomMut = new RandomWrappedValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 0.1d)(GenomeSLocalSigmaFactory)
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeAckley,GAGenomeSigmaFactory[GenomeAckley]] 
      val randomCross = new RandomWrappedValuesCrossOver[GenomeAckley,GAGenomeSigmaFactory[GenomeAckley]] (rate => 0.5d)(GenomeAckleySigmaFactory)
      val selectOp = new BinaryTournamentNSGA2[GenomeAckley,MultiGoal,GAGenomeSigmaFactory[GenomeAckley]]()
     
      // Init algorithms NSGA2 avec les trois types d'operateurs
      val evolutionEngine = new NSGAII[MultiGoal,GenomeAckley,GAGenomeSigmaFactory[GenomeAckley]](softMut,selectOp,randomCross)
       
      // Init archive with individuals, result of evaluation of random genomes previously generated (200 individuals here)
      var individus:IndexedSeq[IndividualMG[GenomeAckley,MultiGoal] with IRanking with IDistance] = genomes.map{g => IndividuFactory.operate(g)}
      
      //Generation evolve
      for (i <- 0 to 8000){
        //Evaluation de la fitness et cr??ation des individu a partir des genomes
        //println("Evaluate fitness to make new population of individuals")
        //var population = new PopulationMG [GenomeSLocal](genomes.map{e => IndividuFactory.build(e)})
        
        // 1- first last arg, here 100 => indicate us the size of sampling of individuals, 
        // so it's the number of individuals we want to conserve after merging new individuals with archive individuals, 
        // This number represent a constant size of our best individuals we want to conserve into archive
        // 2 - second last arg, here 200 => give us the size of offspring generated, based on a matted population, 
        // result of our selection operator on the new best archive after sampling
        var result = evolutionEngine.operate(individus,archive,100,200)
        
        individus = result._1.map{g => IndividuFactory.operate(g)}
        archive = new PopulationMG(result._2)
        //println("new list > " + genomes.map{e=> e.wrappedValues})
       
        //archive = bestPop
     
        println("generation" + i)
>>>>>>> b3847c30c8c7540f0e2b71a2b5cee5af241fc307
      }

      println(archive.map{i => i.fitness.toString})
    }
  }
}