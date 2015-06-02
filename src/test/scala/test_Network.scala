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
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import fr.iscpif.mgo.tools.network._

object NetworkSpecification extends Properties("Network") {

  type EdgeDataType = Char

  val sparseDirectedTopology: Gen[Seq[(Int, Int)]] = Gen.sized { size =>
    for {
      nodes <- Gen.choose(0, size)
      probaLink <- Gen.choose(0.0, 1.0)
      random <- Gen.containerOfN[Vector, Double](nodes * nodes, Gen.choose(0.0, 1.0))
    } yield (0 until nodes).combinations(2).toSeq.zip(random).filter { case (_, r) => r < probaLink }.map { case (pairnodes, r) => (pairnodes(0), pairnodes(1)) }
  }

  property("SparseTopology") =
    forAll(sparseDirectedTopology) {
      (t: Seq[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Seq[EdgeDataType]) =>
            {
              val edges = (t zip s) map { case ((n1, n2), e) => (n1, n2, e) }
              val topo = new SparseTopology[Char] {
                val mapin = SparseTopology.mapinFromSeq(edges)
                val mapout = SparseTopology.mapoutFromSeq(edges)
              }
              all(
                edges map {
                  case (innode, outnode, edgedata) =>
                    ((topo.edge(innode, outnode) ?= Some(edgedata)) :| s"edge $outnode,$innode,$edgedata" &&
                      (topo.in(outnode).contains((innode, edgedata))) :| "in nodes" &&
                      (topo.out(innode).contains((outnode, edgedata))) :| "out nodes")
                }: _*)
            }
        }
    }

  val denseDirectedTopology: Gen[Array[Array[EdgeDataType]]] = Gen.sized { size =>
    for {
      matrix <- Gen.containerOfN[Array, Array[EdgeDataType]](size, Gen.containerOfN[Array, EdgeDataType](size, arbitrary[EdgeDataType]))
    } yield matrix
  }

  property("DenseTopology") =
    forAll(denseDirectedTopology) {
      (t: Array[Array[EdgeDataType]]) =>
        val topo = new DenseTopology[EdgeDataType] {
          val matrix = t
        }
        all(topo.matrix.zipWithIndex.map {
          case (row, i) =>
            row.zipWithIndex.map { case (edge, j) => (topo.edge(i, j) ?= Some(edge)) :| "edge" }
        }.flatten: _*) &&
          all(topo.matrix.indices.map {
            j =>
              (t.zipWithIndex.map { case (row, i) => (i, row(j)) }.toSeq ?= topo.in(j)) :| "in"
          }: _*) &&
          all(t.zipWithIndex.map {
            case (row, i) =>
              (row.zipWithIndex.map { case (e, j) => (j, e) }.toSeq ?= topo.out(i)) :| "out"
          }: _*)
    }

  property("DirectedEdges") =
    forAll(sparseDirectedTopology) {
      (t: Seq[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Seq[EdgeDataType]) =>
            {
              val edges = (t zip s) map { case ((n1, n2), e) => (n1, n2, e) }
              val topo = new DirectedEdges[EdgeDataType] {
                def in(u: Int) = edges.filter { case (_, n2, _) => n2 == u }.map { case (n1, _, e) => (n1, e) }
                def out(u: Int) = edges.filter { case (n1, _, _) => n1 == u }.map { case (_, n2, e) => (n2, e) }
              }
              all(edges.map { case (n1, n2, e) => Prop.propBoolean(topo.outedges(n1) contains (n1, n2, e)) }: _*) &&
                all(edges.map { case (n1, n2, e) => Prop.propBoolean(topo.inedges(n2) contains (n1, n2, e)) }: _*) &&
                all(edges.map { case (n1, n2, e) => Prop.propBoolean(topo.outneighbours(n1) contains n2) }: _*) &&
                all(edges.map { case (n1, n2, e) => Prop.propBoolean(topo.inneighbours(n2) contains n1) }: _*)
            }
        }
    }

  val sparseUndirectedTopology: Gen[Seq[(Int, Int)]] = Gen.sized { size =>
    for {
      nodes <- Gen.choose(0, size)
      probaLink <- Gen.choose(0.0, 1.0)
      randomFilter <- Gen.containerOfN[Vector, Double](nodes * (nodes - 1) / 2, Gen.choose(0.0, 1.0))
      randomSwitch <- Gen.containerOfN[Vector, Double](nodes * (nodes - 1) / 2, Gen.choose(0.0, 1.0))
    } yield (0 until nodes)
      .map { i => (i + 1 until nodes).map { (i, _) } }.flatten.toSeq
      .zip(randomFilter zip randomSwitch)
      .filter { case (_, (rf, _)) => rf < probaLink }
      .map { case ((n1, n2), (_, rs)) => if (rs < 0.5) (n1, n2) else (n2, n1) }
  }

  property("UndirectedEdges") =
    forAll(sparseUndirectedTopology) {
      (t: Seq[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Seq[EdgeDataType]) =>
            {
              val testedges = UndirectedEdges.makeSymetric((t zip s) map { case ((n1, n2), e) => (n1, n2, e) })
              val topo = new UndirectedEdges[EdgeDataType] {
                def out(u: Int) =
                  testedges.filter { case (n1, _, _) => n1 == u }.map { case (_, n2, e) => (n2, e) }

              }
              all(testedges.map { case (n1, n2, e) => Prop.propBoolean(topo.edges(n1) contains (n1, n2, e)) && Prop.propBoolean(topo.edges(n2) contains (n2, n1, e)) }: _*) &&
                all(testedges.map { case (n1, n2, e) => Prop.propBoolean(topo.neighbours(n1) contains n2) && Prop.propBoolean(topo.neighbours(n2) contains n1) }: _*)
            }
        }
    }

  // property("NeuralNetwork query") = 

  // property("creation Undirected Sparse") =
  //   forAll(Gen.choose(1, maxDims), Gen.choose(1, maxPoints)) {
  //     (ndims: Int, npoints: Int) =>
  //       forAll(Gen.containerOfN[Vector, Vector[Double]](npoints, Gen.containerOfN[Vector, Double](ndims, Gen.choose(-1000.0, 1000.0)))) {
  //         (points: Seq[Seq[Double]]) =>
  //           val tree = KDTree(points)
  //           val treeSeqGrouped: Map[Seq[Double], Seq[Seq[Double]]] = tree.toSeq.groupBy[Seq[Double]]((a: Seq[Double]) => a)
  //           val pointsGrouped: Map[Seq[Double], Seq[Seq[Double]]] = points.groupBy[Seq[Double]]((a: Seq[Double]) => a)
  //           s"tree sequence: ${tree.toSeq}\npoints: $points" |: pointsGrouped.forall(kv => kv._2.size == treeSeqGrouped(kv._1).size)
  //       }
  //   }

}

