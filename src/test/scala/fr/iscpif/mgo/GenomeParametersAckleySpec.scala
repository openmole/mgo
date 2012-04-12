//
//package fr.iscpif.mgo
//
//import org.scalatest.FlatSpec
//import org.scalatest.matchers.ShouldMatchers
///*import de.erichseifert.gral.data.DataSeries
//import de.erichseifert.gral.data.DataTable
//import de.erichseifert.gral.io.plots.DrawableWriterFactory
//import de.erichseifert.gral.plots.XYPlot
//import de.erichseifert.gral.plots.points.PointRenderer*/
//import java.awt.Color
//import java.io.File
//import java.io.FileOutputStream
//import java.util.Random
//import org.scalatest.junit.JUnitRunner
//import org.junit.runner.RunWith
//
//import fr.iscpif.mgo._
//import fr.iscpif.mgo.ga.algorithm.GAGenomeWithSigma
//import fr.iscpif.mgo.ga.algorithm.GAGenomeWithSigmaFactory
//import fr.iscpif.mgo.ga._
//import fr.iscpif.mgo.ga.operators.crossover._
//import fr.iscpif.mgo.ga.operators.mutation._
//import fr.iscpif.mgo.ga.operators._
//import fr.iscpif.mgo.ga._
//import fr.iscpif.mgo.ga.GAFitness._
//import fr.iscpif.mgo.ga.algorithm.NSGAII
//import scala.math._
//import fr.iscpif.mgo.tools.FileUtils
//import fr.iscpif.mgo.tools.Scaling._
//
//import java.awt.BorderLayout
//
//import javax.swing.JFrame
//import javax.swing.SwingConstants
//import javax.swing.SwingUtilities
//import javax.swing.plaf.basic.BasicArrowButton
//
////import net.ericaro.surfaceplotter._
//
//
//import java.util.Random
//
//@RunWith(classOf[JUnitRunner])
//class GenomeParametersAckleySpec extends FlatSpec with ShouldMatchers{
//
//  "GenomeParametersAckleySpec " should "create and initialize with good values" in {
//
//
//    val factory = new GAGenomeWithSigmaFactory(2)
//
//    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
//    def evaluator(inGenome: GAGenomeWithSigma) = {
//      // Nombre de dimensions de la fonction = nombre de gene dans le genome
//      val genomeSize:Double = inGenome.values.size
//
//      //val max:Double = 1
//      //val min:Double = 0
//      val max = 32
//      val min = -32
//
//      //println((genome.values ++ genome.sigma).map{ScalingEngine.scale(_,max, min,boundaryMax,boundaryMin)}.toString)
//
//      val a = inGenome.values.map{x => x.scale(min, max)}.map{x => pow(x,2.)}.sum //sum(x(i)^2)
//      val b = inGenome.values.map{x=> x.scale(min, max)}.map{x => cos(2.*Pi*x)}.sum //sum(cos(2*Pi*x(i)
//      val exp1 = exp( (-0.2) * sqrt((1./genomeSize.toDouble)*a))
//      val exp2 = exp((1./genomeSize.toDouble)*b)
//      val fx = 20.+ math.E - (20. * exp1) - exp2
//
//      new GAFitness {
//        val values = IndexedSeq(fx)
//      }
//
//    }
//
//    implicit val rng = new Random
//    val evolutionEngine = NSGAII.sigma(0.9)
//
//    val individus = (0 until 50).map{_ => factory.random}.map{g => Individual(g, evaluator)}
//    evolutionEngine(individus, factory, evaluator, 1).foreach{i => println(i.fitness.values)}
//  }
//}
