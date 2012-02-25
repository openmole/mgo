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
import de.erichseifert.gral.plots.areas.AreaRenderer
import de.erichseifert.gral.plots.areas.DefaultAreaRenderer2D
import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D
import de.erichseifert.gral.plots.lines.LineRenderer
import de.erichseifert.gral.plots.points.PointRenderer
import de.erichseifert.gral.util.GraphicsUtils
import java.awt.Color
import java.awt.Rectangle
import java.awt.geom.AffineTransform
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
    
    def evaluator(agenome: GenomeZDT1) = {
      
      // Nombre de dimensions de la fonction = nombre de gene dans le genome
      val genomeSize:Double = agenome.values.size
      
      val max:Double = 1
      val min:Double = 0
        
      var f0:Double = agenome.values(0).scale(min,max)
      var g:Double = evalG(agenome.values.map{x=>x.scale(min,max)})
      var h:Double = evalH(f0,g)
      var f1:Double = h * g
      
      //println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)        
      
       //println(agenome.values.toString)
       /*if (agenome.values(0).isNaN || agenome.values(1).isNaN){
         
       println("------ >> EVALUATION << -------------")
       println( "f0 > " + f0)
       println( "g > " + g)
       println( "h > " + h)
       println( "f1 > " + f1)
       println("-------------------------------------")
       }*/
       
      new Individual[GenomeZDT1, GAFitness] {
        def genome = agenome
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
      var g = (gValues.slice(1,gValues.size)).sum
      val constante = (9.0 / (gValues.size-1))
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
    
      
      //val randomMut = new RandomWrappedValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 0.1d)(GenomeSLocalSigmaFactory)
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeZDT1, GenomeZDT1Factory] 
      val sbxCross = new SBXBoundedCrossover[GenomeZDT1, GenomeZDT1Factory](0.9)

      // Init algorithms NSGA2 avec les trois types d'operateurs
      val evolutionEngine = new NSGAII(softMut, sbxCross)
      
      // Premier tour, obligatoire pour l'initiatlisation des premier individu
      val individus = (0 until 50).map{_ => factory.buildRandomGenome}.map{g => evaluator(g)}
      
      val archive = evolutionEngine(individus, factory, evaluator, 10)
      
      println(archive.map{i => i.fitness.toString})
      archive
    }
   
    def printFile(path:String, archive:IndexedSeq[Individual[GenomeZDT1, GAFitness]])={
        
      import Individual._
      
      var trueFront = FileUtils.readFront(path)
      val simulFront:Array[Array[Double]] = archive.toMatrix
      val dest1 = new File("/tmp/zdt1.png")
      val dest2 = new File("/tmp/zdt2.png")
      
      println("simulFront Size =" + simulFront.size )
      println("trueFront Size =" + trueFront.size)
      var data = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double],classOf[java.lang.Double], classOf[java.lang.Double])
      for(i <- 0 to trueFront.size - 1) {
        data.add(trueFront(i)(0), trueFront(i)(1),simulFront(i)(0),simulFront(i)(1))
      }
      
      val factory = DrawableWriterFactory.getInstance()  
      val writer  = factory.get("image/png")
      
      val data1 = new DataSeries("True Front", data, 0, 1)
      val data2 = new DataSeries("Simul Front", data, 2, 3)
      val plot1 = new XYPlot(data1)
      val plot2 = new XYPlot(data2) 
      
      plot1.getPointRenderer(data1).setSetting(PointRenderer.COLOR,  new Color(77,77,77))
      plot2.getPointRenderer(data2).setSetting(PointRenderer.COLOR,  new Color(187,200,7))
      
      writer.write(plot1, new FileOutputStream(dest1),600,800)
      writer.write(plot2, new FileOutputStream(dest2),600,800)
    }

    def main = {    
      var path = "/home/srey/TRAVAUX/THESE/REPOSITORY_GIT/mgo/data/ZDT1.pf"
      var bestArchive = initTest
      printFile(path,bestArchive)
    }
    
     // main
    
  } 
}
