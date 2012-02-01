
package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import de.erichseifert.gral.data.DataSeries
import de.erichseifert.gral.data.DataTable
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
import org.openmole.tools.mgo.ga.operators._
import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo.ga.algorithm.NSGAII
import scala.math._
import org.openmole.tools.mgo.tools.FileUtils
import org.openmole.tools.mgo.tools.Scaling._

import java.awt.BorderLayout

import javax.swing.JFrame
import javax.swing.SwingConstants
import javax.swing.SwingUtilities
import javax.swing.plaf.basic.BasicArrowButton

import net.ericaro.surfaceplotter._


import java.util.Random

@RunWith(classOf[JUnitRunner])
class GenomeParametersAckleySpec extends FlatSpec with ShouldMatchers{

  "GenomeParametersAckleySpec " should "create and initialize with good values" in {
    
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
     
    def initTest():IndexedSeq[Individual[GenomeAckley, GAFitness]] = {
      
      implicit val aprng = new Random
      implicit val function: Random => Double = arpng => arpng.nextFloat
    
      // Init random population
      var genomes: IndexedSeq[GenomeAckley] = (0 until 500).map{_ => factory.buildRandomGenome}
    
      //val randomMut = new RandomWrappedValuesMutation[GenomeSLocal,GAGenomeSigmaFactory[GenomeSLocal]] (rate => 0.1d)(GenomeSLocalSigmaFactory)
      val softMut = new CoEvolvingSigmaValuesMutation[GenomeAckley, GenomeAckleyFactory] 
      val sbxCross = new SBXBoundedCrossover[GenomeAckley, GenomeAckleyFactory](0.9)

      // Init algorithms NSGA2 avec les trois types d'operateurs
      val evolutionEngine = new NSGAII(softMut, sbxCross)
       
      // Premier tour, obligatoire pour l'initiatlisation des premier individu
      var individus = evolutionEngine.select(genomes.map{g => evaluator(g)}, genomes.size)
      
      //Generation evolve
      val archive = (0 to 15).foldLeft(individus){
        (acc, gen) => 
          val result = evolutionEngine(acc, factory,evaluator)
          println("generation" + gen)
          printFile(result,gen)
          result
      }
      archive
    }
    
      def printFile(archive:IndexedSeq[Individual[GenomeAckley, GAFitness]],generation:Integer)={

      import Individual._
      
      val simulFront = archive.arrayWithGenome(-32,32)
      
      /*for(i <- 0 to simulFront.size - 1) {
        //fx,val1, val2
        println(simulFront(i)(0),simulFront(i)(1),simulFront(i)(2))
      }*/
      
      val dest1 = new File("/tmp/Ackley01_"+generation+".png")
      val dest2 = new File("/tmp/Ackley02_"+generation+".png")
      val dest3 = new File("/tmp/Ackley12_"+generation+".png")
      
      println("simulFront Size =" + simulFront.size )

      var data = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double], classOf[java.lang.Double])
      for(i <- 0 to simulFront.size - 1) {
        data.add(simulFront(i)(0),simulFront(i)(1),simulFront(i)(2))
      }
      
      val factory = DrawableWriterFactory.getInstance()  
      val writer  = factory.get("image/png")
      
      val data1 = new DataSeries("Simul Front", data, 0, 1)
      val data2 = new DataSeries("Simul Front", data, 0, 2)
      val data3 = new DataSeries("Simul Front", data, 1, 2)
      
      val plot1 = new XYPlot(data1)
      val plot2 = new XYPlot(data2)
      val plot3 = new XYPlot(data3)
      
      plot1.getPointRenderer(data1).setSetting(PointRenderer.COLOR,  new Color(77,77,77))
      plot2.getPointRenderer(data2).setSetting(PointRenderer.COLOR,  new Color(77,77,77))
      plot3.getPointRenderer(data3).setSetting(PointRenderer.COLOR,  new Color(77,77,77))
      
      writer.write(plot1, new FileOutputStream(dest1),600,800)
      writer.write(plot2, new FileOutputStream(dest2),600,800)
      writer.write(plot3, new FileOutputStream(dest3),600,800)
    }
     
    def main = {
    var bestArchive = initTest()
    //Write the best front
    bestArchive.toCsv(new File("/tmp/archive.csv"),bestArchive.arrayWithGenome(-32,32))
    //print3D(bestArchive)
    }
    
    /*def print3D(archive:IndexedSeq[Individual[GenomeAckley, GAFitness]]) = {
           val jsp = new JSurfacePanel()
      jsp.setTitleText("Hello")
      val jf = new JFrame("test")

      jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      jf.getContentPane().add(jsp, BorderLayout.CENTER)
      jf.pack()
      jf.setVisible(true)
      
      def f1(x:Float,y:Float)= Math.sin(x*x+y*y)/(x*x+y*y)
      def f2(x:Float,y:Float)= Math.sin(x*x-y*y)/(x*x+y*y)
      
    }
    
    def mainPrint3D(archive:IndexedSeq[Individual[GenomeAckley, GAFitness]]) = {
      SwingUtilities.invokeLater(new Runnable() {                        
                        def run() =  print3D(archive) 
                        
                })
    }*/
    
    main
    
  }
}
