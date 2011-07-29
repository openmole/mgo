
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.evolution._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.mg.IndividualMG
import org.openmole.tools.mgo.mg.PopulationMG
import org.openmole.tools.mgo.domination.diversity._
import org.openmole.tools.mgo.model._
import org.openmole.tools.mgo.ga.operators._
import org.openmole.tools.mgo.genomefactory._
import org.openmole.tools.mgo.domination.diversity._
import scala.math._
import org.openmole.tools.mgo.tools.ScalingEngine

import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersSpec extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    
    ////////////////////////////////
    // GENOME SLOCAL
    ////////////////////////////////    
    class GenomeSLocal(
      override val values : IndexedSeq[Double],
      override val sigma  : IndexedSeq[Double]) 
    extends GAGenome with SigmaParameters {
      
      def this (v : IndexedSeq[Double]) = 
        this (v.take (v.size/2), v.takeRight (v.size/2))
       
    }
    
    ////////////////////////////////
    // GENOME SIGMA FACTORY
    ////////////////////////////////
    implicit object GenomeSLocalSigmaFactory extends GenomeSigmaFactory [GenomeSLocal] {
      override def buildGenome(v : IndexedSeq[Double]): GenomeSLocal = {
        new GenomeSLocal (v)
      }
      
      override def buildFromValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(values,genome.sigma)
      }
      
      override def buildFromWrappedValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values ++ values)
      }
        
      def buildRandomGenome (implicit aprng : Random) : GenomeSLocal = {
        var values = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
        var sigma = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
        new GenomeSLocal(values, sigma)
      }
    }
    
    ////////////////////////////////
    // GA ENGINE
    ////////////////////////////////
    implicit object EvolveEngine{
    
     val randomMut = 
        new RandomWrappedValuesMutation[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] //(rate => 0.5d,GenomeSLocalSigmaFactory)
      val softMut = 
        new EvolvingSoftGaussianMutation[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] //(GenomeSLocalSigmaFactory)
      val randomCross = 
        new RandomWrappedValuesCrossOver[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] //(GenomeSLocalSigmaFactory)

      // Init evolution engine
      //FIXME : randomCross ne renvoie qu'un bout du genome du fait qu'il utilise la methode build genome ...
      //FIXME : les autres mutations utilise buildFromValues, et retourne deux valeurs, mais que se passe t il si on a n parametres au lieu d'un seul ?
      val evolutionEngine = new EvolutionEngine (randomMut,randomCross,softMut)
      
      // Select function Individual[GenomeDouble,_]
      def select(population: PopulationMG[GenomeSLocal], nbSlot: Int) : Iterable[IndividualMG[GenomeSLocal,_]] = {   
        FitnessByRank.selectByFitnessAndCrowding(population.individuals,nbSlot)
      }
    
      //Evolve function
      def evolve(population: PopulationMG[GenomeSLocal],sizeOfNewPop:Int)(implicit aprng: Random):IndexedSeq[GenomeSLocal] = {
        evolutionEngine.apply(population.toGenomes.toIndexedSeq,sizeOfNewPop)
      }
    
      //New archive
      def mergePopulation(initialPopulation: PopulationMG[GenomeSLocal], archive: PopulationMG[GenomeSLocal]): PopulationMG[GenomeSLocal] = { 
        if(archive.individuals.size > 0)
          new PopulationMG[GenomeSLocal](initialPopulation.individuals ++ archive.individuals)
        else 
          new PopulationMG[GenomeSLocal](initialPopulation.individuals)
      }
    }
    
    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
      object IndividuFactory {
      def build(genome:GenomeSLocal):IndividualMG[GenomeSLocal,MultiGoal] = {
    
        // Nombre de dimensions de la fonction = nombre de gene dans le genome
        val genomeSize = genome.values.size
        println("genomeSize > " + genomeSize)
        println("genomeValues > " + genome.values.toString)
        
        val max:Double = 0 
        val min:Double = 1
        val boundaryMax:Double = 30 
        val boundaryMin:Double = -30
       
        println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)
                                      
        
        val a = genome.values.map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.map{x => x * x}.sum //sum(x(i)^2)
        val b = genome.values.map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.map{x => cos(2*Pi*x)}.sum //sum(cos(2*Pi*x(i)
        val t1 = 20 + exp(1) - 20 * exp( -0.2 * sqrt((1/genomeSize)*a))
        val t2 = exp((1/genomeSize)*b) 
        val fx = 20 + exp(1) - 20 * exp( -0.2 * sqrt((1/genomeSize)*a))-exp((1/genomeSize)*b)      
        
        println("------ >> EVALUATION << -------------")
        println( "a > " + a)
        println( "b > " + b)
        println( "t1 > " + t1)
        println( "t2 > " + t2)
        println( "fx > " + fx)
        println("-------------------------------------")
  
        val fitness = MultiGoal.buildDouble(fx)
        return (new IndividualMG(genome,fitness))
      }
    }
    
    // MAIN 
    implicit val aprng = new Random
    
    implicit val function: Random => Double = arpng => arpng.nextFloat

    
    // Init random population
    var genomes:IndexedSeq[GenomeSLocal] = (0 until 100).map{_ => GenomeSLocalSigmaFactory.buildRandomGenome}
    
    
    // Init de l'archive population, vide au premier tour
    var archive = new PopulationMG[GenomeSLocal](IndexedSeq.empty)
    
    //Generation evolve
    for (i <- 0 to 50){
      //Evaluation de la fitness et création des individu a partir des genomes
      println("Evaluate fitness to make new population of individuals")
      var population = new PopulationMG [GenomeSLocal](genomes.map{e => IndividuFactory.build(e)})
    
      // Merge de l'ancienne et de la nouvelle population
     println("Merge archive and new population of individuals")
     var mergedPopulation = EvolveEngine.mergePopulation(population, archive)
      
      //Selection des meilleurs individus dans la population merged
      println("Select the best population")
      var bestPop = new PopulationMG[GenomeSLocal](EvolveEngine.select(mergedPopulation,10).toIndexedSeq)
      
      //Multiplication des meilleurs éléments entres eux pour former une nouvelle population de genome
      println("Generate a new list of genomes with this best population")
      genomes = EvolveEngine.evolve(bestPop,100)
      
     archive = bestPop
     
    }
  
  }
}