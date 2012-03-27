/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ga.selection._
import fr.iscpif.mgo.ga.operators._
import scala.math._
import fr.iscpif.mgo.tools.Scaling._
import java.util.Random

@RunWith(classOf[JUnitRunner])
class SCHSpec extends FlatSpec with ShouldMatchers{

  "SCHSpec" should "create and initialize with good values" in {
    
    
    ////////////////////////////////
    // ZDT1
    ////////////////////////////////    
    /*
     class GenomeSLocal(
      override val values : IndexedSeq[Double],
      override val sigma  : IndexedSeq[Double]
      ) 
    extends GAGenome with SigmaParameters {

      def this (v : IndexedSeq[Double]) = 
        this (v.slice(from = 0, until = 1), v.slice(from = 1, until= 2))
      
      override val wrappedValues = values ++ sigma
        
      def print = println(wrappedValues)
       
    }
    */
    
    ////////////////////////////////
    // GENOME SIGMA FACTORY
    ////////////////////////////////
    /*
      
     implicit object GenomeSLocalSigmaFactory extends GAGenomeSigmaFactory [GenomeSLocal] {
      override def buildGenome(v : IndexedSeq[Double]): GenomeSLocal = {
        new GenomeSLocal (v)
      }
      
      override def buildFromValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(values,genome.sigma)
      }
      
      override def buildFromWrappedValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values ++ values)
      }
      
      override def buildFromSigmaValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values,values)
      }
      
      def buildRandomGenome (implicit aprng : Random) : GenomeSLocal = {
        var values = (0 until 1).map{_ => aprng.nextDouble}.toIndexedSeq
        var sigma = (0 until 1).map{_ => aprng.nextDouble}.toIndexedSeq
        new GenomeSLocal(values, sigma)
      }
    }
    */
    
    ////////////////////////////////
    // GA ENGINE
    ////////////////////////////////
    /*
     implicit object EvolveEngine{
    
     val randomMut = 
        new RandomValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 1d,GenomeSLocalSigmaFactory)
            
     val strictDominant = new StrictDominant
     val fitnessByRank = new FitnessByRank(strictDominant)

      // Init evolution engine
      val evolutionEngine = new EvolutionEngine (randomMut)
      
      // Select function Individual[GenomeDouble,_]
      def select(population: PopulationMG[GenomeSLocal], nbSlot: Int) : Iterable[IndividualMG[GenomeSLocal,_]] = {
        fitnessByRank.selectByFitnessAndCrowding(population.individuals,nbSlot)
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
    */
    /*
     object IndividuFactory {
      def build(genome:GenomeSLocal):IndividualMG[GenomeSLocal,MultiGoal] = {
    
        // Nombre de dimensions de la fonction = nombre de gene dans le genome
        val genomeSize:Double = genome.values.size
        
        val max:Double = 1
        val min:Double = 0
        val boundaryMax:Double = 1000 
        val boundaryMin:Double = -1000
        
        val valGen1 = ScalingEngine.scale(genome.values(0),max, min,boundaryMax,boundaryMin)
       
        var f0:Double = valGen1 * valGen1
        var f1:Double = (valGen1 - 2) * (valGen1 - 2)
 
        
        //println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)        
        
        println("genome equal")
        println(genome.values.toString)
        println("------ >> EVALUATION << -------------")
        println("Valgen = " + valGen1)
        println( "f0 > " + f0)
        println( "f1 > " + f1)
        println("-------------------------------------")
  
        val fitness = MultiGoal.buildDouble(f0,f1)
        return (new IndividualMG(genome,fitness))
      }
    }
    */
    // MAIN 
   /* 
    initTest
    
    def initTest ={
      
    
    implicit val aprng = new Random
    
    implicit val function: Random => Double = arpng => arpng.nextFloat

    
    // Init random population
    var genomes:IndexedSeq[GenomeSLocal] = (0 until 25).map{_ => GenomeSLocalSigmaFactory.buildRandomGenome}
    
    
    // Init de l'archive population, vide au premier tour
    var archive = new PopulationMG[GenomeSLocal](IndexedSeq.empty)
    
    //Generation evolve
    for (i <- 0 to 1000){
      //Evaluation de la fitness et cr??ation des individu a partir des genomes
      //println("Evaluate fitness to make new population of individuals")
      var population = new PopulationMG [GenomeSLocal](genomes.map{e => IndividuFactory.build(e)})
    
      // Merge de l'ancienne et de la nouvelle population
     //println("Merge archive and new population of individuals")
     var mergedPopulation = EvolveEngine.mergePopulation(population, archive)
      
      //Selection des meilleurs individus dans la population merged
      //println("Select the best population")
      var bestPop = new PopulationMG[GenomeSLocal](EvolveEngine.select(mergedPopulation,5).toIndexedSeq)
      
      //Multiplication des meilleurs ??l??ments entres eux pour former une nouvelle population de genome
      //println("Generate a new list of genomes with this best population")
      genomes = EvolveEngine.evolve(bestPop,25)
      
      //println("new list > " + genomes.map{e=> e.wrappedValues})
       
     archive = bestPop
     
     println("generation" + i)
    }
    
    println(archive.individuals.map{e => e.multiGoal.toString})
  
  }
  */
  
  }
  
}
