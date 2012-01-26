/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import de.erichseifert.gral.data._
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.points.PointRenderer
import java.awt.Color
import java.io.File
import java.io.FileOutputStream
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.operators.crossover._
import org.openmole.tools.mgo.ga.operators.mutation._
import org.openmole.tools.mgo.selection._
import org.openmole.tools.mgo.ga.algorithm._
import org.openmole.tools.mgo.ga.operators._
import scala.math._
import org.openmole.tools.mgo.tools.FileUtils
import org.openmole.tools.mgo.tools.Scaling._

import java.util.Random

@RunWith(classOf[JUnitRunner])
class ZDTSpec extends FlatSpec with ShouldMatchers{

  "ZDTSpec " should "create and initialize with good values" in {
    
    trait GenomeZDT1 extends GAGenome with SigmaParameters {
      def wrappedValues = values ++ sigma
    }
    
    class GenomeZDT1Factory extends GAGenomeFactory[GenomeZDT1] with GASigmaParametersFactory [GenomeZDT1] {
      override def buildGenome(v : IndexedSeq[Double]) = 
        new GenomeZDT1 {
          val values = v.slice(0, 2)
          val sigma = v.slice(2, 4)
        } 
      
      override def buildFromValues(genome: GenomeZDT1, _values: IndexedSeq [Double]) = 
        new GenomeZDT1 {
          val values = _values
          val sigma = genome.sigma
        }

      override def buildFromSigma(genome: GenomeZDT1, _sigma: IndexedSeq [Double]) = 
        new GenomeZDT1 {
          val values = genome.values
          val sigma = _sigma
        }
      
      def buildRandomGenome (implicit aprng : Random):GenomeZDT1 = 
        new GenomeZDT1 {
          val values = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
          val sigma = (0 until 2).map{_ => aprng.nextDouble}.toIndexedSeq
        } 

    }
    
    ////////////////////////////////
    // ZDT1
    ////////////////////////////////    

    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
    
    val factory = new GenomeZDT1Factory
    
    def evaluator(genome: GenomeZDT1) = {
      
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
      
      new Individual[GenomeZDT1, GAFitness] {
        def genome = genome
        def fitness = new GAFitness {
          val fitness = IndexedSeq(f0,f1)
        }
      }
    }
      
    /**
     * Returns the value of the ZDT1 function G.
     * @param decisionVariables The decision variables of the solution to 
     * evaluate.
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
      
 
    def initTest():IndexedSeq[Individual[GenomeZDT1, GAFitness]] = {
       
      implicit val aprng = new Random
      implicit val function: Random => Double = arpng => arpng.nextFloat
    
      // Init random population
      var genomes: IndexedSeq[GenomeZDT1] = (0 until 100).map{_ => factory.buildRandomGenome}
    
      //val randomMut = new RandomWrappedValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 0.1d)(GenomeSLocalSigmaFactory)
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeZDT1, GenomeZDT1Factory] 
      val sbxCross = new SBXBoundedCrossover[GenomeZDT1, GenomeZDT1Factory](0.9)

      // Init algorithms NSGA2 avec les trois types d'operateurs
      val evolutionEngine = new NSGAII(softMut, sbxCross)
       println("START TURN")
      // Premier tour, obligatoire pour l'initiatlisation des premier individu
      var individus = evolutionEngine.select(genomes.map{g => evaluator(g)}, genomes.size)
      println("END TURN")
      //Generation evolve
     
      val archive = (0 to 800).foldLeft(individus){
        (acc, gen) => 
        val result = evolutionEngine(acc, factory,evaluator)
        println("generation" + gen)
        result
      }
      println("end generation")
      
      println(archive.map{i => i.fitness.toString})
      return archive
    
    }
   
    def printFile(path:String, archive:IndexedSeq[Individual[GenomeZDT1, GAFitness]])={
        
      var trueFront = FileUtils.readFront(path)
      val simulFront:Array[Array[Double]] = FileUtils.convertIndividualsFrontToMatrix(archive)
      val dest = new File("/tmp/essai.png")
        
      
      var data = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double],classOf[java.lang.Double], classOf[java.lang.Double])
      for(i <- 0 to trueFront.size - 1) {
        data.add(trueFront(i)(0), trueFront(i)(1),simulFront(i)(0),simulFront(i)(1))
      }
      
      val factory = DrawableWriterFactory.getInstance()  
      val writer  = factory.get("image/png")
      
      val data1 = new DataSeries("True Front", data, 0, 1)
      val data2 = new DataSeries("Simul Front", data, 2, 3)
      val plot = new XYPlot(data1,data2)
      
        
      val color1 = new Color(0.0f, 0.3f, 1.0f) 
      val color2 = new Color(0.3f, 0.5f, 0.1f)
      plot.getPointRenderer(data1).setSetting(PointRenderer.COLOR, color1)
      plot.getPointRenderer(data2).setSetting(PointRenderer.COLOR, color2)
       
      writer.write(plot, new FileOutputStream(dest),400,400)
      
    }
     
    def main = {
      
    
    //var path = "/home/srey/TRAVAUX/THESE/REPOSITORY_GIT/mgo/data/ZDT1.pf"
   
    var bestArchive = initTest()

    //printFile(path,bestArchive)
    
    }
    
    main
    
  } 
}
