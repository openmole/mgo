
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

import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersSpec extends FlatSpec with ShouldMatchers{

  "GenomeParameters " should "create and initialize with good values" in {
    
    
    
    class GenomeSLocal(
      override val values : IndexedSeq[Double],
      override val sigma  : IndexedSeq[Double]) 
    extends GAGenome with SigmaParameters {
      
      def this (v : IndexedSeq[Double]) = 
        this (v.take (v.size/2), v.takeRight (v.size/2))
    }
    
    class GenomeSLocalSigmaFactory extends GenomeSigmaFactory[GenomeSLocal] {
      override def buildGenome(v : IndexedSeq[Double]): GenomeSLocal = {
        new GenomeSLocal (v)
      }
      
      override def buildFromValues(genome: GenomeSLocal, sigmaValues: IndexedSeq [Double]): GenomeSLocal = {
        new GenomeSLocal(genome.values,sigmaValues)
      }
        
      def buildRandomGenome (implicit aprng : Random) : GenomeSLocal = {
        var values = (0 until 5).map{_ => aprng.nextDouble}.toIndexedSeq
        var sigma = (0 until 5).map{_ => aprng.nextDouble}.toIndexedSeq
        new GenomeSLocal(values, sigma)
      }
    }
    
    class EvolveEngine(rng:Random, gaGenomeFactory:GenomeSLocalSigmaFactory){
    
      // Init operators
      val randomMut = new RandomValuesMutation[GenomeSLocal,GenomeSLocalSigmaFactory](rate => 0.3d,gaGenomeFactory)
      val softMut = new EvolvingSoftGaussianMutation[GenomeSLocal,GenomeSLocalSigmaFactory](gaGenomeFactory)
      val randomCross = new RandomCrossOver[GenomeSLocal,GenomeSLocalSigmaFactory](gaGenomeFactory)
    
      // Init evolution engine
      val evolutionEngine = new EvolutionEngine[GenomeSLocal,GenomeSLocalSigmaFactory](randomMut,softMut,randomCross)
      
// Select function Individual[GenomeDouble,_]
      def select(population: PopulationMG[GenomeSLocal],nbSlot: Int):Iterable[IndividualMG[GenomeSLocal,_]] = {   
        FitnessByRank.selectByFitnessAndCrowding(population.individuals,nbSlot)
      }
     // def selectByFitnessAndCrowding[MG <: MultiGoalLike](toSelect: IndexedSeq[MG], resPopSize: Int): IndexedSeq[MG] = {

      //Evolve function
      def evolve(population: PopulationMG[GenomeSLocal],sizeOfNewPop:Int):IndexedSeq[GenomeSLocal] = {
        evolutionEngine.apply(population.toGenomes.toIndexedSeq,sizeOfNewPop)(rng)
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
        val a = genome.values.map{x => x * x}.sum //sum(x(i)^2)
        val b = genome.values.map{x => cos(2*Pi*x)}.sum //sum(cos(2*Pi*x(i)
        val fx = 20 + exp(1) - 20*exp(-0.2*sqrt((1/genomeSize)*a))-exp((1/genomeSize)*b)      
    
        println("------ >> EVALUATION << -------------")
        println( "fx > " + fx)
        println("-------------------------------------")
  
        val fitness = MultiGoal.buildDouble(fx)
        return (new IndividualMG(genome,fitness))
      }
    }
    
    // MAIN 
    
    val rng = new Random
    
    //Init Factory
    val gaGenomeFactory = new GenomeSLocalSigmaFactory
    
    //Init Engine 
    val evolveEngine = new EvolveEngine(rng:Random, gaGenomeFactory)
    
    // Init random population
    var genomes = (0 until 100).map{_ => gaGenomeFactory.buildRandomGenome(rng)}
    
    
    val populationStart = new PopulationMG [GenomeSLocal](genomes.map{e => IndividuFactory.build(e)})

    //Generation evolve
    /*for (i <- 0 to 50){
      evolveEngine.merge
      evolveEngine.select
      evolveEngine.evolve
      
    }*/

    
  
  }
}
