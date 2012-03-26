/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.awt.Color
import java.awt.Rectangle
import java.awt.geom.AffineTransform
import java.io.File
import java.io.FileOutputStream
import java.util.Random
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ga.operators.crossover._
import fr.iscpif.mgo.ga.operators.mutation._
import fr.iscpif.mgo.ga.selection._
import fr.iscpif.mgo.ga.algorithm._
import fr.iscpif.mgo.ga.operators._
import scala.math._
import fr.iscpif.mgo.tools.FileUtils
import fr.iscpif.mgo.tools.Scaling._


import java.util.Random

@RunWith(classOf[JUnitRunner])
class ZDTSpec extends FlatSpec with ShouldMatchers{

  "ZDTSpec " should "create and initialize with good values" in {
    
      
    ////////////////////////////////
    // ZDT1
    ////////////////////////////////    

    // http://tracer.lcc.uma.es/problems/ackley/ackley.html
    
    val factory = new GAGenomeWithSigmaFactory(2)
    
    def evaluator(agenome: GAGenomeWithSigma) = {
      
      // Nombre de dimensions de la fonction = nombre de gene dans le genome
      val genomeSize:Double = agenome.values.size
      
      val max = 1.
      val min = 0.
        
      val f0 = agenome.values(0).scale(min,max)
      val g = evalG(agenome.values.map{_.scale(min, max)})
      val h = evalH(f0, g)
      val f1 = h * g
      
      new GAFitness {
        val fitness = IndexedSeq(f0, f1)
      }
      
    }

    /**
     * Returns the value of the ZDT1 function G.
     * @param decisionVariables The decision variables of the solution to 
     * evaluate.
     */
    def evalG(gValues:IndexedSeq[Double])=
      (gValues.slice(1, gValues.size)).sum * (9.0 / (gValues.size-1))  + 1.0
  

    
    /**
     * Returns the value of the ZDT1 function H.
     * @param f First argument of the function H.
     * @param g Second argument of the function H.
     */
    def evalH(f: Double, g: Double) = 1.0 - sqrt(f/g)
      
 
       
    implicit val aprng = new Random
      
    // Init algorithms NSGA2 avec les trois types d'operateurs
    val evolutionEngine = new SigmaNSGAII(0.9)
      
    // Premier tour, obligatoire pour l'initiatlisation des premier individu
    val individus = (0 until 50).map{_ => factory.random}.map{g => Individual(g, evaluator)}.toIndexedSeq
      
    //Too slow for a unit test
    //val archive = evolutionEngine(individus, factory, evaluator, 1)
      
    //println(archive.map{i => i.fitness.toString})
      
    
   
    
  } 
}
