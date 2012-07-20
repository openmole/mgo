///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
package fr.iscpif.mgo.metrics

import collection.mutable._
import collection.immutable.{IndexedSeq => IIndexedSeq}

// REF A RAJOUTER
// BASE SUR LE CODE PYTHON ICI MEME : http://ls11-www.cs.uni-dortmund.de/_media/rudolph/hypervolume/hv_python.zip

object HyperVolume extends App{

  val referencePoint = IndexedSeq(2.0, 2.0, 2.0)
  val front = IndexedSeq(IndexedSeq(1.0, 0.0, 1.0), IndexedSeq(0.0, 1.0, 0.0))

  println("reference Point = " + referencePoint.mkString(" "))
  println("Front = " + front.mkString(" "))
  println("value of volume = " + compute(front) )

  def compute(front: IndexedSeq[IndexedSeq[Double]]):Double = {

    def weaklyDominates(point: IndexedSeq[Double], other: IndexedSeq[Double]): Boolean = {
      for (i <- Range(0, point.size)) {
        if (point(i) > other(i))
          return false
      }
      return true
    }

    var relevantPoints: IndexedSeq[IndexedSeq[Double]] = IndexedSeq.empty

    val dimensions = referencePoint.size

    for (point <- front) {
      if (weaklyDominates(point, referencePoint)) {
        relevantPoints = relevantPoints :+ point
      }
    }

    /*
     relevantPoints.map { _.zipWith(referencePoints).map{ case(c, r) => c - r} }
     */
    //if (relevantPoints.size > 0) {
    relevantPoints = 
      relevantPoints.map {
        point =>
        ArrayBuffer() ++ Range(0, dimensions).map {
          i => point(i) - referencePoint(i)
        }
      }
    //}
    println("RELEVANT POINTS = " + relevantPoints)
    var list = preProcess(relevantPoints)
    println("LIST = " + list)

    var bounds = IndexedSeq.fill(dimensions) {
      -1.0e308
    }

    def hvRecursive(dimIndex: Int, length: Int, bounds: IndexedSeq[Double]): Double = {

      var hvol = 0.0
      var sentinel = list.sentinel
      var newLength = length

      if (newLength == 0) {
        hvol
      } else if (dimIndex == 0) {
        -sentinel.next(0).cargo(0)
      } else if (dimIndex == 1) {
        var q = sentinel.next(1)
        var h = q.cargo(0)
        var p = q.next(1)
        while (p != sentinel) {
          var pCargo = p.cargo
          hvol += h * (q.cargo(1) - pCargo(1))
          if (pCargo(0) < h) {
            h = pCargo(0)
          }
          q = p
          p = q.next(1)
        }
        hvol += h * q.cargo(1)
        hvol
      }
      else {

        var p = sentinel
        var q = p.prev(dimIndex)

        while (q.cargo != None) {
          if (q.ignore < dimIndex) {
            q.ignore = 0
          }
          q = q.prev(dimIndex)
        }

        q = p.prev(dimIndex)

        while (newLength > 1 && (q.cargo(dimIndex) > bounds(dimIndex) || q.prev(dimIndex).cargo(dimIndex) >= bounds(dimIndex))) {
          p = q
          list.remove(p, dimIndex, bounds)
          q = p.prev(dimIndex)
          newLength = newLength - 1
        }

        var qArea = q.area
        var qCargo = q.cargo
        var qPrevDimIndex = q.prev(dimIndex)

        if (newLength > 1) {
          hvol = qPrevDimIndex.volume(dimIndex) + qPrevDimIndex.area(dimIndex) * (qCargo(dimIndex) - qPrevDimIndex.cargo(dimIndex))
        } else {
          qArea = (IndexedSeq(1.0) ++ Range(0, dimIndex).map {
              i => qArea(i) * -qCargo(i)
            })

          //qArea.slice(1, dimIndex + 1) = Range(0, dimIndex).map{ i => qArea(i) * -qCargo(i)}
          //qArea[ 1: dimIndex + 1] =[qArea[i] * -qCargo[i] for i in xrange(dimIndex)]
        }

        q.volume(dimIndex) = hvol

        if (q.ignore >= dimIndex) {
          qArea(dimIndex) = qPrevDimIndex.area(dimIndex)
        } else {
          qArea(dimIndex) = hvRecursive(dimIndex - 1, newLength, bounds)
          if (qArea(dimIndex) <= qPrevDimIndex.area(dimIndex)) {
            q.ignore = dimIndex
          }
        }

        while (p != sentinel) {
          var pCargoDimIndex = p.cargo(dimIndex)
          hvol += q.area(dimIndex) * (pCargoDimIndex - q.cargo(dimIndex))
          bounds(dimIndex) = pCargoDimIndex
          list.reinsert(p, dimIndex, bounds)
          newLength += 1
          q = p
          p = p.next(dimIndex)
          q.volume(dimIndex) = hvol

          if (q.ignore >= dimIndex) {
            q.area(dimIndex) = q.prev(dimIndex).area(dimIndex)
          } else {
            q.area(dimIndex) = hvRecursive(dimIndex - 1, newLength, bounds)
            if (q.area(dimIndex) <= q.prev(dimIndex).area(dimIndex)) {
              q.ignore = dimIndex
            }
          }
        }
        hvol -= q.area(dimIndex) * q.cargo(dimIndex)
        hvol

      }
    }

    return hvRecursive(dimensions - 1, relevantPoints.size, bounds)
  }

  def preProcess(front: IndexedSeq[IndexedSeq[Double]]): MultiList = {
    val dimensions = referencePoint.size
    var nodeList = new MultiList(dimensions)
    println("FRONT PROCESS = " + front)
    var nodes = front.map {
      point => new Node(dimensions, point)
    }

    println("NODES = " + nodes)

    Range(0, dimensions).map {
      i =>
      nodes = sortByDimension(nodes, i)
      nodeList.extend(nodes, i)
      println( "i  = " + i)
    }
    println("sentinel = " + nodeList.sentinel.toString())
    nodeList
  }

  def sortByDimension(nodes: IndexedSeq[Node], i: Int): IndexedSeq[Node] = 
    nodes.sortBy(_.cargo(i))
  /*{
    //build a list of tuples of(point[i], node)
    var decorated = nodes.map {
      node => (node.cargo(i), node)
    }
    decorated.sortBy(_._1)
    println("decorated = " + decorated.toString)

    decorated.map {
      case (_, node) => node
    }
  }*/


  class Node(numberLists: Int, var cargo: IndexedSeq[Double] = IndexedSeq.empty) {

    var next: IndexedSeq[Node] = IndexedSeq.empty
    var prev: IndexedSeq[Node] = IndexedSeq.empty
    var ignore = 0
    var area = IndexedSeq.fill(numberLists) {
      0.0
    }
    var volume = IndexedSeq.fill(numberLists) {
      0.0
    }

    println("CREATE NODE OK + dim = " + numberLists )

    override def toString(): String = cargo.toString()

  }

  class MultiList(numberLists: Int) {


    var sentinel = new Node(numberLists)

    sentinel.next = IndexedSeq.fill(numberLists) {
      sentinel
    }
    sentinel.prev = IndexedSeq.fill(numberLists) {
      sentinel
    }

    def len = numberLists

    def getLength(i: Int): Int = {
      var length = 0
      var node = sentinel.next(i)
      while (node != sentinel) {
        length += 1
        node = node.next(i)
      }
      length
    }

    def append(node: Node, index: Int) = {
      val lastButOne = sentinel.prev(index)
      node.next(index) = sentinel
      node.prev(index) = lastButOne //set the last element as the new one
      sentinel.prev(index) = node
      lastButOne.next(index) = node
    }


    def extend(nodes: IndexedSeq[Node], index: Int) = {

      for (node <- nodes) {
        val lastButOne = sentinel.prev(index)
        node.next(index) = sentinel
        node.prev(index) = lastButOne //set the last element as the new one
        sentinel.prev(index) = node
        lastButOne.next(index) = node
      }
    }

    def remove(node: Node, index: Int, bounds: IndexedSeq[Double]) = {
      for (i <- Range(0, index)) {
        val predecessor = node.prev(i)
        val successor = node.next(i)
        predecessor.next(i) = successor
        successor.prev(i) = predecessor
        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }

      }
    }

    def reinsert(node: Node, index: Int, bounds: IndexedSeq[Double]) = {
      for (i <- Range(0, index)) {
        node.prev(i).next(i) = node
        node.next(i).prev(i) = node
        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }
      }

    }
  }
}

//class HyperVolume(fileName:String) {
//
//  val loadParetoTrueFront = FileUtils.readFront(fileName)
//  
//  def oppositeByFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
//    
//    val nbObjective = paretoFront.head.fitness.values.size
//    
//    var arrayOfMin = new Array[Double](nbObjective)
//    var arrayOfMax = new Array[Double](nbObjective)
//    
//    for (curDim <- 0 until nbObjective) {  
//      var curList = paretoFront.sortBy(_.fitness.values(curDim))
//      arrayOfMin(curDim) = curList.head.fitness.values(curDim)
//      arrayOfMax(curDim) = curList.last.fitness.values(curDim) 
//    }
//    
//    Seq(arrayOfMin,arrayOfMax)
//  }
//  
//  def toNormalize(value:Double,min:Double,max:Double):Double={
//    ( value - min ) / (max - min)
//  }
//  
//  def invertedFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
//    
//    val nbObjective = paretoFront.head.fitness.values.size
//    var invertedFront = IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]()
//    
//    for (curObjectives <- 0 until nbObjective) {  
//      invertedFront :+ paretoFront.map{i => 
//        new Individual[GAGenome, GAFitness] {
//          def genome = i.genome
//          def fitness = new GAFitness {
//            val values = i.fitness.values.map{e => 
//              if (e <= 1.0 && e >= 0.0) 1.0 - e
//              else if (e > 1.0) 0.0
//              else 1.0 //if (e < 0.0 )
//            }
//          }
//        }
//      }
//    }
//    invertedFront
//  }
//  
//  def normalizedFront(paretoFront:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
//    
//    val arrayOfMinMaxValues = oppositeByFront(paretoFront)
//    val arrayMin = arrayOfMinMaxValues(0)
//    val arrayMax = arrayOfMinMaxValues(1)
//    
//    var normalizedFront = IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]()
//    val nbObjective = paretoFront.head.fitness.values.size
//    
//    // On re-calcule un front d'individu normalisé a partir du front actuel(a priori le meilleur ...), selon le min max de chaque objectif
//    for (curObjectives <- 0 until nbObjective) {  
//      normalizedFront :+ paretoFront.map{i => 
//        new Individual[GAGenome, GAFitness] {
//          def genome = i.genome
//          def fitness = new GAFitness {
//            val values = i.fitness.values.map{toNormalize(_,arrayMin(curObjectives),arrayMax(curObjectives))}
//          }
//        }
//      }
//    }
//    normalizedFront
//  }
//  
//  /**
//   * R version http://www.statistik.tu-dortmund.de/~olafm/software/emoa/
//   * http://ls11-www.cs.tu-dortmund.de/rudolph/hypervolume/start
//   *
//   * This class implements the hypervolume indicator. The code is the a Java version
//   * of the original metric implementation by Eckart Zitzler.
//   * It can be used also as a command line program just by typing
//   * $java jmetal.qualityIndicator.Hypervolume <solutionFrontFile> <trueFrontFile> <numberOfOjbectives>
//   * Reference: E. Zitzler and L. Thiele
//   *           Multiobjective Evolutionary Algorithms: A Comparative Case Study 
//   *           and the Strength Pareto Approach,
//   *           IEEE Transactions on Evolutionary Computation, vol. 3, no. 4, 
//   *           pp. 257-271, 1999.
//   */
//
//  // Peut etre serait il mieux de convertir notre indexedSeq d'invidual en matrice de double ... et de re
//  // reprendre l'algorithme de Zitzler par la suite ...
//  /*def calculateHypervolume(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking],
//                           noPoints:Int,
//                           noObjectives:Int) ={
//    var n:Int = noPoints
//    var volume:Double = 0
//    var distance:Double = 0
//    
//    while (n > 0) {
//      
//     // renvoie le front d'individu non dominé, et non pas une valeur donc avec un rank = 0
//     var noNonDominatedSize = noNondominatedPoints.size - front.filter{i => i.rank == 0}
//     var noNondominatedPoints = front.filter{i => i.rank == 0}
//     
//     
//      var tempVolume : Double = 0
//      var tempDistance:Double
//      
//      if (noObjectives < 3)
//        if (noNondominatedPoints.size < 1)  
//          
//      
//      
//     }  
//  }
//  
//  def surfaceUnchangedTo(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking], noPoints : Int, objective:Int):Double={
//     (front.head.fitness.fitness(objective).min
//  }
//    
//    
//
//  def getHyperVolume(front:IndexedSeq[Individual[GAGenome, GAFitness] with Distance with Ranking]) = {
//    
//    //val normalizedFront = normalizedFront(front)
//    //val invertedFront = invertedFront(front)
//    
//    //Fonction recursive pour le calcul de l'hypervolume'
//    
//  }*/
//  
//}
