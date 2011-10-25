/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

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
import org.openmole.tools.mgo.selection._
import org.openmole.tools.mgo.domination._
import org.openmole.tools.mgo.model._
import org.openmole.tools.mgo.ga.operators._
import org.openmole.tools.mgo.genomefactory._
import scala.math._
import org.openmole.tools.mgo.tools.ScalingEngine

import java.util.Random

@RunWith(classOf[JUnitRunner])
class ZDTSpec extends FlatSpec with ShouldMatchers{

  "ZDTSpec " should "create and initialize with good values" in {
    
    
    ////////////////////////////////
    // ZDT1
    ////////////////////////////////    
    class GenomeSLocal(
      override val values : IndexedSeq[Double],
      override val sigma  : IndexedSeq[Double]
      ) 
    extends GAGenome with SigmaParameters {

      def this (v : IndexedSeq[Double]) = 
        this (v.slice(from = 0, until = 30), v.slice(from = 30, until= 60))
      
      override val wrappedValues = values ++ sigma
        
      def print = println(wrappedValues)
       
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
      
      override def buildFromSigmaValues(genome: GenomeSLocal, values: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values,values)
      }
      
      def buildRandomGenome (implicit aprng : Random) : GenomeSLocal = {
        var values = (0 until 30).map{_ => aprng.nextDouble}.toIndexedSeq
        var sigma = (0 until 30).map{_ => aprng.nextDouble}.toIndexedSeq
        new GenomeSLocal(values, sigma)
      }
    }
    
    ////////////////////////////////
    // GA ENGINE
    ////////////////////////////////
    implicit object EvolveEngine{
    
     val randomMut = 
        new RandomWrappedValuesMutation[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] (rate => 0.75d,GenomeSLocalSigmaFactory)
      val softMut = 
        new CoEvolvingSigmaValuesMutation[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] //(GenomeSLocalSigmaFactory)
      val randomCross = 
        new RandomWrappedValuesCrossOver[GenomeSLocal,GenomeSigmaFactory[GenomeSLocal]] //(GenomeSLocalSigmaFactory)

      // Init evolution engine
      val evolutionEngine = new EvolutionEngine (softMut,randomCross,randomMut)
      
      // Select function Individual[GenomeDouble,_]
      def select(population: PopulationMG[GenomeSLocal], nbSlot: Int) : Iterable[IndividualMG[GenomeSLocal,_]] = {
              
        val fitnessByRank = new FitnessByRank(new StrictDominant)
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
    
    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
      object IndividuFactory {
      def build(genome:GenomeSLocal):IndividualMG[GenomeSLocal,MultiGoal] = {
    
        // Nombre de dimensions de la fonction = nombre de gene dans le genome
        val genomeSize:Double = genome.values.size
        
        val max:Double = 1
        val min:Double = 0
      
        var f0:Double = genome.values(0)
        var g:Double = evalG(genome.values)
        var h:Double = evalH(f0,g)
        var f1:Double = h * g
   
        //println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)        
        
        println("genome equal")
        println(genome.values.toString)
        println("------ >> EVALUATION << -------------")
        println( "f0 > " + f0)
        println( "g > " + g)
        println( "h > " + h)
        println( "f1 > " + f1)
        println("-------------------------------------")
  
        val fitness = MultiGoal.buildDouble(f0,f1)
        return (new IndividualMG(genome,fitness))
      }
      
      
  /**
   * Returns the value of the ZDT1 function G.
   * @param decisionVariables The decision variables of the solution to 
   * evaluate.
   * @throws JMException 
   */
  def evalG(gValues:IndexedSeq[Double]):Double={
    var g = (gValues.slice(1,30)).sum
    val constante = (9.0 / (30-1))
    g = constante * g
    g = g + 1.0
    return g
  } // evalG
    
  /**
   * Returns the value of the ZDT1 function H.
   * @param f First argument of the function H.
   * @param g Second argument of the function H.
   */
  def evalH(f:Double, g:Double):Double ={
    var h:Double = 0.0
    h = 1.0 - sqrt(f/g);
    return h        
  } // evalH
      
      
    }
    
    // MAIN 
    //initTest
    
    def initTest ={
      
    
    implicit val aprng = new Random
    
    implicit val function: Random => Double = arpng => arpng.nextFloat

    
    // Init random population
    var genomes:IndexedSeq[GenomeSLocal] = (0 until 50).map{_ => GenomeSLocalSigmaFactory.buildRandomGenome}
    
    
    // Init de l'archive population, vide au premier tour
    var archive = new PopulationMG[GenomeSLocal](IndexedSeq.empty)
    
    //Generation evolve
    for (i <- 0 to 250){
      //Evaluation de la fitness et cr??ation des individu a partir des genomes
      //println("Evaluate fitness to make new population of individuals")
      var population = new PopulationMG [GenomeSLocal](genomes.map{e => IndividuFactory.build(e)})
    
      // Merge de l'ancienne et de la nouvelle population
     //println("Merge archive and new population of individuals")
     var mergedPopulation = EvolveEngine.mergePopulation(population, archive)
      
      //Selection des meilleurs individus dans la population merged
      //println("Select the best population")
      var bestPop = new PopulationMG[GenomeSLocal](EvolveEngine.select(mergedPopulation,25).toIndexedSeq)
      
      //Multiplication des meilleurs ??l??ments entres eux pour former une nouvelle population de genome
      //println("Generate a new list of genomes with this best population")
      genomes = EvolveEngine.evolve(bestPop,50)
      
      //println("new list > " + genomes.map{e=> e.wrappedValues})
       
     archive = bestPop
     
     println("generation" + i)
    }
    
    println(archive.individuals.map{e => e.multiGoal.toString})
  
  }
  }
  
}
