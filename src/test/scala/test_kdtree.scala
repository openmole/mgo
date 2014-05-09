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

package test

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import fr.iscpif.mgo.tools.KDTree
import fr.iscpif.mgo.tools.distance._
import collection.immutable._

object KDTreeSpecification extends Properties("KDTree") {

  val maxPoints = 100
  val maxDims = 10
  val maxK = 5

  property("transpose") =
    forAll(Gen.choose(1, maxDims), Gen.choose(1, maxPoints)) {
      (ndims: Int, npoints: Int) =>
        forAll(Gen.listOfN(npoints, Gen.listOfN(ndims, Gen.choose(-1000.0, 1000.0)))) {
          (points: Seq[Seq[Double]]) =>
            KDTree.transpose(KDTree.transpose(points)) ?= points
        }
    }

  property("creation") =
    forAll(Gen.choose(1, maxDims), Gen.choose(1, maxPoints)) {
      (ndims: Int, npoints: Int) =>
        forAll(Gen.containerOfN[Vector, Vector[Double]](npoints, Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)))) {
          (points: Seq[Seq[Double]]) =>
            val tree = KDTree(points)
            val treeSeqGrouped: Map[Seq[Double], Seq[Seq[Double]]] = tree.toSeq.groupBy[Seq[Double]]((a: Seq[Double]) => a)
            val pointsGrouped: Map[Seq[Double], Seq[Seq[Double]]] = points.groupBy[Seq[Double]]((a: Seq[Double]) => a)
            s"tree sequence: ${tree.toSeq}\npoints: $points" |: pointsGrouped.forall(kv => kv._2.size == treeSeqGrouped(kv._1).size)
        }
    }

  property("nearest neighbour") =
    forAll(Gen.choose(1, maxDims), Gen.choose(1, maxPoints)) {
      (ndims: Int, npoints: Int) =>
        forAll(Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)),
          Gen.containerOfN[Vector, Vector[Double]](npoints, Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)))) {
            (query: Seq[Double], points: Seq[Seq[Double]]) =>
              val tree: KDTree = KDTree(points)
              val kdtreeNearest: Seq[Double] = tree.nearest(query)
              val bruteForceNearest: Seq[Double] = {
                points.reduce[Seq[Double]]((a, b) => if (tree.distance(a, query) < tree.distance(b, query)) a else b).toSeq
              }
              (s"ndims=$ndims npoints=$npoints\nquery=$query\npoints=$points\n$tree") |: (kdtreeNearest ?= bruteForceNearest)
          }
    }

  property("k nearest neighbour") =
    forAll(Gen.choose(1, maxDims), Gen.choose(1, maxPoints), Gen.choose(1, maxK)) {
      (ndims: Int, npoints: Int, k: Int) =>
        forAll(Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)),
          Gen.containerOfN[Vector, Vector[Double]](npoints, Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)))) {
            (query: Seq[Double], points: Seq[Seq[Double]]) =>
              val tree: KDTree = KDTree(points)
              val kdtreeNearest: Seq[Seq[Double]] = tree.knearest(k, query)
              val bruteForceNearest: Seq[Seq[Double]] =
                points.foldLeft(Vector[Seq[Double]]()) {
                  case (knn: Vector[Seq[Double]], cur: Seq[Double]) =>
                    ((cur +: knn) sortWith { tree.distance(_, query) < tree.distance(_, query) }).take(k)
                }
              // val bruteForceNearest: Seq[Seq[Double]] =
              //   points.sortWith(tree.distance(_,query) < )

              (s"ndims=$ndims npoints=$npoints k=$k\nquery=$query\npoints=$points\n$tree\n" +
                s"kdtreeNearest=$kdtreeNearest\ndistances from query=${kdtreeNearest.map(tree.distance(_, query))}\n" +
                s"bruteForceNearest=$bruteForceNearest\ndistances from query=${bruteForceNearest.map(tree.distance(_, query))}") |: (kdtreeNearest ?= bruteForceNearest)
          }
    }
}

