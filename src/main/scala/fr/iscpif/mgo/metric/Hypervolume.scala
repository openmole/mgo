/*
 * Copyright (C) 2012 Sebastien Rey
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.metric

import collection.mutable.{IndexedSeq => MIndexedSeq}
import scala.collection.mutable.ArrayBuffer
import fr.iscpif.mgo.Dominance
import math._

// A translation/adaptation based on the python source code by Simon Wessing :
// http://ls11-www.cs.uni-dortmund.de/_media/rudolph/hypervolume/hv_python.zip

/**
 * Hypervolume computation based on variant 3 of the algorithm in the paper:
 * C. M. Fonseca, L. Paquete, and M. Lopez-Ibanez. An improved dimension-sweep
 * algorithm for the hypervolume indicator. In IEEE Congress on Evolutionary
 * Computation, pages 1157-1163, Vancouver, Canada, July 2006.
 * 
 * FIXE: The implementation is ugly, as the algorithm as directly been translated
 * from python
 * 
 */
object Hypervolume {

  /**
   * Compute the nadir of a set of points
   * 
   * @param point a set of points
   * @return the nadir point
   */
  def nadir(points: IndexedSeq[IndexedSeq[Double]]) =
    points.reduce {
      (i1, i2) => (i1 zip i2).map {
        case (i1, i2) => max(i1, i2)
      }
    }

  /**
   * Compute the hypervolume that is dominated by a non-dominated front.
   * Before the HV computation, front and reference point are translated, so
   * that the reference point is [0, ..., 0].
   * 
   * @param front the parato front
   * @param reference point the reference point for computing the volume from
   * this point to the front
   * @return the hypervolume
   */
  def apply(front: IndexedSeq[IndexedSeq[Double]], referencePoint:Seq[Double], d: Dominance): Double = {
    def dominates(point: Seq[Double], other: Seq[Double]): Boolean = 
      d.isDominated(other, point)

    val dimensions = referencePoint.size
    
    val relevantPoints = front.filter(dominates(_, referencePoint)).map{
      point => (point zip referencePoint).map{case(p, r) => p - r}
    }    

    val list = preProcess(relevantPoints, referencePoint)

    val bounds = MIndexedSeq.fill(dimensions) {
      -1.0e308
    }

    /**
     * Recursive call to hypervolume calculation.
     *
     * In contrast to the paper, the code assumes that the reference point
     * is [0, ..., 0]. This allows the avoidance of a few operations.
     */
    def hvRecursive(dimIndex: Int, length: Int, bounds: MIndexedSeq[Double]): Double = {

      var hvol = 0.0
      var sentinel = list.sentinel
      var newLength = length

      if (newLength == 0) {
        hvol
      } else if (dimIndex == 0) {
        sentinel.next(0) match {
         case None => 0.0
         case Some(n) => -n.cargo(0)
       }
      } else if (dimIndex == 1) {

        //Transform q option to node
        var q:Node = sentinel.next(1).get

        var h:Double = q.cargo(0)
        //Transform p option to node
        var p:Node = q.next(1).get

        while (p != sentinel) {

          var pCargo = p.cargo
          hvol += h * (q.cargo(1) - pCargo(1))
          if (pCargo(0) < h) {
            h = pCargo(0)
          }
          q = p
          p = q.next(1).get
        }
        hvol += h * q.cargo(1)
        hvol
      }
      else {

        var p: Node = sentinel
        //Transform q option to node
        var q: Node = p.prev(dimIndex).get

        while (!q.cargo.isEmpty) {
          if (q.ignore < dimIndex) {
            q.ignore = 0
          }
          q = q.prev(dimIndex).get
        }

        q = p.prev(dimIndex).get

        while (newLength > 1 && (q.cargo(dimIndex) > bounds(dimIndex) || (q.prev(dimIndex).get).cargo(dimIndex) >= bounds(dimIndex))) {
          p = q
          list.remove(p, dimIndex, bounds)
          q = p.prev(dimIndex).get
          newLength = newLength - 1
        }

        var qPrevDimIndex = q.prev(dimIndex).get
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
          p = p.next(dimIndex).get
          q.volume(dimIndex) = hvol

          if (q.ignore >= dimIndex) {
            q.area(dimIndex) = (q.prev(dimIndex).get).area(dimIndex)
          } else {
            q.area(dimIndex) = hvRecursive(dimIndex - 1, newLength, bounds)
            if (q.area(dimIndex) <= (q.prev(dimIndex).get).area(dimIndex)) {
              q.ignore = dimIndex
            }
          }
        }
        hvol -= q.area(dimIndex) * q.cargo(dimIndex)
        hvol

      }
    }

    //MAIN COMPUTE
    return hvRecursive(dimensions - 1, relevantPoints.size, bounds)
  }

  /* Sets up the list data structure needed for calculation. */
  def preProcess(front: IndexedSeq[IndexedSeq[Double]],referencePoint: Seq[Double]): MultiList = {
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

  /* Sorts the list of nodes by the i -th value of the contained points. */
  def sortByDimension(nodes: IndexedSeq[Node], i: Int): IndexedSeq[Node] = nodes.sortBy(_.cargo(i))
  
  class Node(numberLists: Int, val cargo: IndexedSeq[Double] = IndexedSeq.empty) {

    var next: MIndexedSeq[Option[Node]] = MIndexedSeq.fill(numberLists){None}
    var prev: MIndexedSeq[Option[Node]] = MIndexedSeq.fill(numberLists) {None}
    var ignore = 0

    var area = MIndexedSeq.fill(numberLists) {
      0.0
    }

    var volume = MIndexedSeq.fill(numberLists) {
      0.0
    }

   override def toString = cargo.toString

  }

  /**
   * A special data structure needed by FonsecaHyperVolume.
   *
   * It consists of several doubly linked lists that share common nodes. So,
   * every node has multiple predecessors and successors, one in every list.
   */
  class MultiList(numberLists: Int) {

    var sentinel = new Node(numberLists)
    sentinel.next = MIndexedSeq.fill(numberLists) {
      Some(sentinel)
    }
    sentinel.prev = MIndexedSeq.fill(numberLists) {
      Some(sentinel)
    }

    /* Returns the number of lists that are included in this MultiList. */
    def len = numberLists

    /* Returns the length of the i-th list. */
    def getLength(i: Int): Int = {
      var length = 0
      var node = sentinel.next(i)
      
      while (node.get != sentinel) {
        length += 1
        node = node.get.next(i)
      }
      length
    }

    /* Appends a node to the end of the list at the given index. */
    def append(node: Node, index: Int) = {
      val lastButOne = sentinel.prev(index)
      node.next(index) = Some(sentinel)
      node.prev(index) = lastButOne //set the last element as the new one
      sentinel.prev(index) = Some(node)
      lastButOne match {
        case None => 
        case Some(n) => n.next(index) = Some(node)
      }
    }

    /* Extends the list at the given index with the nodes. */
    def extend(nodes: IndexedSeq[Node], index: Int) = {

      for (node <- nodes) {
        val lastButOne = sentinel.prev(index)
        node.next(index) = Some(sentinel)
        node.prev(index) = lastButOne //set the last element as the new one
        sentinel.prev(index) = Some(node)
        lastButOne match {
          case None => 
          case Some(n) => n.next(index) = Some(node)
        }
      }
    }

    /* Removes and returns 'node' from all lists in [0, 'index'[. */
    def remove(node: Node, index: Int, bounds: MIndexedSeq[Double]) = {
      for (i <- Range(0, index)) {
        val predecessor = node.prev(i)
        val successor = node.next(i)

        predecessor match {
          case None =>
          case Some(n) => n.next(i) = successor
        }

       successor match {
          case None =>
          case Some(n) => n.prev(i) = predecessor
        }

        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }

      }
    }

    /**
     * Inserts 'node ' at the position it had in all lists in[ 0, 'index '[
     * before it was removed.This method assumes that the next and previous
     * nodes of the node that is reinserted are in the list.
     */
    def reinsert(node: Node, index: Int, bounds: MIndexedSeq[Double]) = {
      for (i <- Range(0, index)) {


        node.prev(i) match {
          case None =>
          case Some(n) => n.next(i) = Some(node)
        }

        node.next(i) match {
          case None =>
          case Some(n) => n.prev(i) = Some(node)
        }

        if (bounds(i) > node.cargo(i)) {
          bounds(i) = node.cargo(i)
        }
      }

    }
  }
}
