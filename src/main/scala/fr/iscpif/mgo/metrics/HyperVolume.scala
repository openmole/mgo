///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
package fr.iscpif.mgo.metrics

//import collection.mutable._
//import collection.immutable.{IndexedSeq => IIndexedSeq}
//import collection.mutable
import collection.mutable.{IndexedSeq => MIndexedSeq}
import scala.collection.mutable.ArrayBuffer
// REF A RAJOUTER
// BASE SUR LE CODE PYTHON ICI MEME : http://ls11-www.cs.uni-dortmund.de/_media/rudolph/hypervolume/hv_python.zip

object HyperVolume extends App{

  
  val referencePoint = IndexedSeq(2.0, 2.0, 2.0)
  //val front = IndexedSeq(IndexedSeq(1.0, 0.0 ,1.0), IndexedSeq(0.0, 1.0, 0.0))
  val front = IndexedSeq(IndexedSeq(0.2, 1.2, 0.4), IndexedSeq(0.2, 0.8, 0.1), IndexedSeq(0.1, 0.2, 0.9), IndexedSeq(0.4, 0.05, 0.2))

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

    val dimensions = referencePoint.size
    
    val relevantPoints = front.filter(weaklyDominates(_, referencePoint)).map{
      point => (point zip referencePoint).map{case(p, r) => p - r}
    }    

    var list = preProcess(relevantPoints)

    var bounds = MIndexedSeq.fill(dimensions) {
      -1.0e308
    }

    def hvRecursive(dimIndex: Int, length: Int, bounds: MIndexedSeq[Double]): Double = {

      var hvol = 0.0
      var sentinel:Node = list.sentinel
      var newLength = length

      if (newLength == 0) {
        hvol
      } else if (dimIndex == 0) {
        sentinel.next(0) match {
         case None => 0.0
         case Some(n:Node) => -n.cargo(0)
       }
      } else if (dimIndex == 1) {

        //Transform q option to node
        var q:Node = sentinel.next(1) match {
          case Some(n:Node) => n
        }

        var h:Double = q.cargo(0)
        //Transform p option to node
        var p:Node = q.next(1) match { case Some(n:Node) => n}

        while (p != sentinel) {

          var pCargo = p.cargo
          hvol += h * (q.cargo(1) - pCargo(1))
          if (pCargo(0) < h) {
            h = pCargo(0)
          }
          q = p
          p = q.next(1) match {
            case Some(n: Node) => n
          }
        }
        hvol += h * q.cargo(1)
        hvol
      }
      else {

        var p: Node = sentinel
        //Transform q option to node
        var q: Node = p.prev(dimIndex) match {
          case Some(n: Node) => n
        }

        while (!q.cargo.isEmpty) {
          if (q.ignore < dimIndex) {
            q.ignore = 0
          }
          q = q.prev(dimIndex) match {
            case Some(n: Node) => n
          }
        }

        q = p.prev(dimIndex) match {
          case Some(n: Node) => n
        }

        while (newLength > 1 && (q.cargo(dimIndex) > bounds(dimIndex) || (q.prev(dimIndex) match { case Some(n: Node) => n }).cargo(dimIndex) >= bounds(dimIndex))) {
          p = q
          list.remove(p, dimIndex, bounds)
          q = p.prev(dimIndex) match {
            case Some(n: Node) => n
          }
          newLength = newLength - 1
        }

        var qPrevDimIndex = q.prev(dimIndex) match {case Some(n: Node) => n }

        if (newLength > 1) {
          hvol = qPrevDimIndex.volume(dimIndex) + qPrevDimIndex.area(dimIndex) * (q.cargo(dimIndex) - qPrevDimIndex.cargo(dimIndex))
        } else {
          // CODE POURRI , A REFAIRE car QAREA est un passage par ref en python ... sert a rien
          q.area(0) = 1.0
          q.area = ArrayBuffer(1.0) ++ (Range(0, dimIndex).map {
              i =>  q.area(i) * -q.cargo(i)
            })
          q.area = q.area

          }

        q.volume(dimIndex) = hvol

        if (q.ignore >= dimIndex) {
          q.area(dimIndex) = qPrevDimIndex.area(dimIndex)
        } else {
          q.area(dimIndex) = hvRecursive(dimIndex - 1, newLength, bounds)
          if (q.area(dimIndex) <= qPrevDimIndex.area(dimIndex)) {
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
          p = p.next(dimIndex) match { case Some(n: Node) => n }
          q.volume(dimIndex) = hvol

          if (q.ignore >= dimIndex) {
            q.area(dimIndex) = (q.prev(dimIndex) match { case Some(n: Node) => n }).area(dimIndex)
          } else {
            q.area(dimIndex) = hvRecursive(dimIndex - 1, newLength, bounds)
            if (q.area(dimIndex) <= (q.prev(dimIndex) match { case Some(n: Node) => n }).area(dimIndex)) {
              q.ignore = dimIndex
            }
          }
        }
        hvol -= q.area(dimIndex) * q.cargo(dimIndex)
        hvol

      }
    }

    //MAIN COMPUTE
    println("START COMPUTATION OF RECURSIVE")
    return hvRecursive(dimensions - 1, relevantPoints.size, bounds)
  }

  def preProcess(front: IndexedSeq[IndexedSeq[Double]]): MultiList = {
    val dimensions = referencePoint.size
    var nodeList = new MultiList(dimensions)

    var nodes = front.map {
      point => new Node(dimensions, point)
    }

    Range(0, dimensions).map {
      i =>
      nodes = sortByDimension(nodes, i)
      nodeList.extend(nodes, i)
    }
    nodeList
  }

  def sortByDimension(nodes: IndexedSeq[Node], i: Int): IndexedSeq[Node] = nodes.sortBy(_.cargo(i))

  class Node(numberLists: Int, var cargo: IndexedSeq[Double] = IndexedSeq.empty) {

    var next: MIndexedSeq[Option[Node]] = MIndexedSeq.fill(numberLists){None}
    var prev: MIndexedSeq[Option[Node]] = MIndexedSeq.fill(numberLists) {None}
    var ignore = 0

    var area = MIndexedSeq.fill(numberLists) {
      0.0
    }

    var volume = MIndexedSeq.fill(numberLists) {
      0.0
    }

   override def toString():String = cargo.toString

  }

  class MultiList(numberLists: Int) {

    var sentinel = new Node(numberLists)
    sentinel.next = MIndexedSeq.fill(numberLists) {
      Some(sentinel)
    }
    sentinel.prev = MIndexedSeq.fill(numberLists) {
      Some(sentinel)
    }

    def len = numberLists

    def getLength(i: Int): Int = {
      var length = 0
      var node = sentinel.next(i)
      while (node != sentinel) {
        length += 1
        node = node match {
          case Some(n: Node) => n.next(i)
        }
      }
      length
    }

    def append(node: Node, index: Int) = {
      val lastButOne = sentinel.prev(index)
      node.next(index) = Some(sentinel)
      node.prev(index) = lastButOne //set the last element as the new one
      sentinel.prev(index) = Some(node)
      lastButOne match {
        case None => println("empty last But One")
        case Some(n:Node) => n.next(index) = Some(node)
      }
    }


    def extend(nodes: IndexedSeq[Node], index: Int) = {

      for (node <- nodes) {
        val lastButOne = sentinel.prev(index)
        node.next(index) = Some(sentinel)
        node.prev(index) = lastButOne //set the last element as the new one
        sentinel.prev(index) = Some(node)
        lastButOne match {
          case None => println("empty last but one")
          case Some(n:Node) => n.next(index) = Some(node)
        }
      }
    }

    def remove(node: Node, index: Int, bounds: MIndexedSeq[Double]) = {
      for (i <- Range(0, index)) {
        val predecessor = node.prev(i)
        val successor = node.next(i)

        predecessor match {
          case None =>
          case Some(n:Node) => n.next(i) = successor
        }

       successor match {
          case None =>
          case Some(n: Node) => n.prev(i) = predecessor
        }

        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }

      }
    }

    def reinsert(node: Node, index: Int, bounds: MIndexedSeq[Double]) = {
      for (i <- Range(0, index)) {


        node.prev(i) match {
          case None =>
          case Some(n:Node) => n.next(i) = Some(node)
        }

        node.next(i) match {
          case None =>
          case Some(n: Node) => n.prev(i) = Some(node)
        }

        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }
      }

    }
  }
}
