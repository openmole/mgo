/*
 * Copyright (C) Guillaume Chérel 2/05/14
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package fr.iscpif.mgo.tools

import scala.collection._
import fr.iscpif.mgo.tools.distance._

/**
 * KD-Tree algorithm from https://en.wikipedia.org/wiki/Kd-tree 5-5-2014
 *
 */

class KDTree(node: Seq[Double], left: Option[KDTree], right: Option[KDTree]) extends EuclideanDistance {
  def nearest(query: Seq[Double], depth: Int = 0): Seq[Double] = {
    val axis = depth % node.size
    val (naturalDirection, unnaturalDirection) = if (query(axis) < node(axis)) (left, right) else (right, left)

    val curBest =
      naturalDirection match {
        case None => node
        case Some(child) => {
          val childBest = child.nearest(query, depth + 1)
          if (distance(query, node) < distance(query, childBest)) node else childBest
        }
      }
    val distCurBest = distance(query, curBest)
    val best =
      if (distCurBest <= (query(axis) - node(axis)).abs) curBest
      else unnaturalDirection match {
        case None => curBest
        case Some(child) => {
          val childBest = child.nearest(query, depth + 1)
          if (distance(query, curBest) <= distance(query, childBest)) curBest else childBest
        }
      }
    best
  }

  def knearest(k: Int, query: Seq[Double], depth: Int = 0): Seq[Seq[Double]] = {
    val axis = depth % node.size
    val (naturalDirection, unnaturalDirection) = if (query(axis) < node(axis)) (left, right) else (right, left)

    val curBest =
      naturalDirection match {
        case None => Vector(node)
        case Some(child) => {
          val childBest = child.knearest(k, query, depth + 1)
          insertInKNearest(childBest, node, query, k)
        }
      }
    val couldBeNearer: Boolean = curBest exists (distance(_, query) > (query(axis) - node(axis)).abs)
    val best =
      if (!couldBeNearer && curBest.size >= k) curBest
      else unnaturalDirection match {
        case None => curBest
        case Some(child) => {
          val childBest = child.knearest(k, query, depth + 1)

          childBest.foldLeft(curBest)((kn, n) => insertInKNearest(kn, n, query, k))
        }
      }
    best
  }

  def insertInKNearest(l: Seq[Seq[Double]], e: Seq[Double], query: Seq[Double], k: Int): Seq[Seq[Double]] =
    (e +: l).sortWith(distance(_, query) < distance(_, query)).take(k)

  // def insertSortedWith(l: Seq[Seq[Double]], e: Seq[Double], lt: (Seq[Double], Seq[Double]) => Boolean): Seq[Seq[Double]] = {
  //   if (l.size == 0) Vector(e)
  //   else if (lt(e, l(0))) e +: l
  //   else l(0) +: insertSortedWith(l.drop(1), e, lt)
  // }

  def toSeq: Seq[Seq[Double]] =
    ((left match {
      case None => Vector[Seq[Double]]()
      case Some(child) => child.toSeq
    }) :+ node) ++ (right match {
      case None => Vector[Seq[Double]]()
      case Some(child) => child.toSeq
    })

  override def toString: String = s"Node($node, $left, $right)"
}

class EmptyTree extends KDTree(Vector[Double](), None, None) {
  override def nearest(query: Seq[Double], depth: Int = 0): Seq[Double] = Vector[Double]()
  override def knearest(k: Int, query: Seq[Double], depth: Int = 0): Seq[Seq[Double]] = Vector[Vector[Double]]()
  override def toString: String = "EmptyTree"
  override def toSeq: Seq[Seq[Double]] = Vector(Vector[Double]())
}

object KDTree {
  /**
   * @param pointList A size N sequence of size K sequences, representing N points in K dimensions
   * @return
   */
  def apply(pointList: Seq[Seq[Double]]) = {
    if (pointList.size == 0) new EmptyTree
    else if (pointList(0).size == 0) new EmptyTree
    else {
      val tPointList = transpose(pointList)
      val sortedDims = tPointList map (argSort)
      build(tPointList, sortedDims, 0)
    }
  }

  def transpose(pointList: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    val k = pointList(0).size
    (0 until k).map(ki => pointList.map(_(ki)))
  }

  def build(pointList: Seq[Seq[Double]], sortedDims: Seq[Vector[Int]], depth: Int): KDTree = {
    val nDims = pointList.size
    val axis = depth % nDims
    val (medInt, medVal) = findLeftMostMedian(pointList(axis), sortedDims(axis))
    //println(s"medInt: $medInt medVal: $medVal")
    val split: Seq[(Vector[Int], Vector[Int])] = sortedDims.map(splitIdArrays(pointList(axis), _, medInt))
    //println("pointList:"+pointList)
    //println("sortedDims:"+sortedDims)
    //println("split:"+split)
    new KDTree(pointList.map(_(medInt)),
      if (split(0)._1.size > 0) Some(build(pointList, split.map(_._1), depth + 1)) else None,
      if (split(0)._2.size > 0) Some(build(pointList, split.map(_._2), depth + 1)) else None)
  }

  /**
   * split the sorted indices according to threshold, such that elements in res._1 refer to points < threshold, and res._2
   * to points >= threshold. The element splitPoint excluded from the result
   */
  def splitIdArrays(points: Seq[Double], sortedIndices: Vector[Int], splitPoint: Int): (Vector[Int], Vector[Int]) =
    sortedIndices.foldLeft((Vector[Int](), Vector[Int]())) {
      case (split: (Seq[Int], Seq[Int]), i: Int) =>
        if (points(i) < points(splitPoint)) (split._1 :+ i, split._2)
        else if (i == splitPoint) split
        else (split._1, split._2 :+ i)
    }

  def findLeftMostMedian(points: Seq[Double], sortedIndices: Vector[Int]): (Int, Double) = {
    var medInd = sortedIndices.size / 2
    while ((medInd > 0) && (points(sortedIndices(medInd - 1)) == points(sortedIndices(medInd)))) { medInd -= 1 }
    (sortedIndices(medInd), points(sortedIndices(medInd)))
  }

  /* return the indices of the given array referencing the arrays elements in increasing order
   */
  def argSort(a: Seq[Double]): Vector[Int] = (a zip a.indices).sortBy(_._1).map(_._2).toVector
}