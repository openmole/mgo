/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.metrics

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ga.selection._
import fr.iscpif.mgo.tools._
import scala.math._

class HyperVolume(fileName:String) {

  val loadParetoTrueFront = FileUtils.readFront(fileName)
  
  def oppositeByFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
    
    val nbObjective = paretoFront.head.fitness.values.size
    
    var arrayOfMin = new Array[Double](nbObjective)
    var arrayOfMax = new Array[Double](nbObjective)
    
    for (curDim <- 0 until nbObjective) {  
      var curList = paretoFront.sortBy(_.fitness.values(curDim))
      arrayOfMin(curDim) = curList.head.fitness.values(curDim)
      arrayOfMax(curDim) = curList.last.fitness.values(curDim) 
    }
    
    Seq(arrayOfMin,arrayOfMax)
  }
  
  def toNormalize(value:Double,min:Double,max:Double):Double={
    ( value - min ) / (max - min)
  }
  
  def invertedFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
    
    val nbObjective = paretoFront.head.fitness.values.size
    var invertedFront = IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]()
    
    for (curObjectives <- 0 until nbObjective) {  
      invertedFront :+ paretoFront.map{i => 
        new Individual[GAGenome, GAFitness] {
          def genome = i.genome
          def fitness = new GAFitness {
            val values = i.fitness.values.map{e => 
              if (e <= 1.0 && e >= 0.0) 1.0 - e
              else if (e > 1.0) 0.0
              else 1.0 //if (e < 0.0 )
            }
          }
        }
      }
    }
    invertedFront
  }
  
  def normalizedFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
    
    val arrayOfMinMaxValues = oppositeByFront(paretoFront)
    val arrayMin = arrayOfMinMaxValues(0)
    val arrayMax = arrayOfMinMaxValues(1)
    
    var normalizedFront = IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]()
    val nbObjective = paretoFront.head.fitness.values.size
    
    // On re-calcule un front d'individu normalisé a partir du front actuel(a priori le meilleur ...), selon le min max de chaque objectif
    for (curObjectives <- 0 until nbObjective) {  
      normalizedFront :+ paretoFront.map{i => 
        new Individual[GAGenome, GAFitness] {
          def genome = i.genome
          def fitness = new GAFitness {
            val values = i.fitness.values.map{toNormalize(_,arrayMin(curObjectives),arrayMax(curObjectives))}
          }
        }
      }
    }
    normalizedFront
  }
  
  /**
   * R version http://www.statistik.tu-dortmund.de/~olafm/software/emoa/
   * http://ls11-www.cs.tu-dortmund.de/rudolph/hypervolume/start
   *
   * This class implements the hypervolume indicator. The code is the a Java version
   * of the original metric implementation by Eckart Zitzler.
   * It can be used also as a command line program just by typing
   * $java jmetal.qualityIndicator.Hypervolume <solutionFrontFile> <trueFrontFile> <numberOfOjbectives>
   * Reference: E. Zitzler and L. Thiele
   *           Multiobjective Evolutionary Algorithms: A Comparative Case Study 
   *           and the Strength Pareto Approach,
   *           IEEE Transactions on Evolutionary Computation, vol. 3, no. 4, 
   *           pp. 257-271, 1999.
   */

  // Peut etre serait il mieux de convertir notre indexedSeq d'invidual en matrice de double ... et de re
  // reprendre l'algorithme de Zitzler par la suite ...
  /*def calculateHypervolume(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking],
                           noPoints:Int,
                           noObjectives:Int) ={
    var n:Int = noPoints
    var volume:Double = 0
    var distance:Double = 0
    
    while (n > 0) {
      
     // renvoie le front d'individu non dominé, et non pas une valeur donc avec un rank = 0
     var noNonDominatedSize = noNondominatedPoints.size - front.filter{i => i.rank == 0}
     var noNondominatedPoints = front.filter{i => i.rank == 0}
     
     
      var tempVolume : Double = 0
      var tempDistance:Double
      
      if (noObjectives < 3)
        if (noNondominatedPoints.size < 1)  
          
      
      
     }  
  }
  
  def surfaceUnchangedTo(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking], noPoints : Int, objective:Int):Double={
     (front.head.fitness.fitness(objective).min
  }
    
    

  def getHyperVolume(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
    
    //val normalizedFront = normalizedFront(front)
    //val invertedFront = invertedFront(front)
    
    //Fonction recursive pour le calcul de l'hypervolume'
    
  }*/
  
}
