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

  type NodeDataType = String
  type EdgeDataType = Char

  val sparseDirectedTopology: Gen[Vector[(Int, Int)]] = Gen.sized { size =>
    sparseDirectedTopology(size)
  }

  def sparseDirectedTopology(size: Int): Gen[Vector[(Int, Int)]] =
    for {
      nodes <- Gen.choose(0, size)
      probaLink <- Gen.choose(0.0, 1.0)
      random <- Gen.containerOfN[Vector, Double](nodes * nodes, Gen.choose(0.0, 1.0))
    } yield (0 until nodes).combinations(2).toVector.zip(random).filter { case (_, r) => r < probaLink }.map { case (pairnodes, r) => (pairnodes(0), pairnodes(1)) }

  property("SparseTopology") =
    forAll(sparseDirectedTopology) {
      (t: Vector[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Vector[EdgeDataType]) =>
            {
              val edges = (t zip s) map { case ((n1, n2), e) => (n1, n2, e) }
              val topo = new SparseTopology[Char] {
                val mapin = SparseTopology.mapinFrom(edges)
                val mapout = SparseTopology.mapoutFrom(edges)
              }
              all(
                edges map {
                  case (innode, outnode, edgedata) =>
                    ((topo.edge(innode, outnode) ?= Some(edgedata)) :| s"edge $outnode,$innode,$edgedata" &&
                      (topo.in(outnode).contains((innode, edgedata))) :| "in nodes" &&
                      (topo.out(innode).contains((outnode, edgedata))) :| "out nodes")
                }: _*) &&
                (edges.forall { case (innode, outnode, edgedata) => topo.iteredges.contains((innode, outnode, edgedata)) } :| "all test edges are in iteredges") &&
                (topo.iteredges.forall { case (innode, outnode, edgedata) => edges.contains((innode, outnode, edgedata)) } :| "all iteredges are in test edges")
            }
        }
    }

  def denseDirectedTopology(maxSize: Int): Gen[Vector[Vector[EdgeDataType]]] =
    for {
      size <- Gen.choose(0, maxSize)
      matrix <- Gen.containerOfN[Vector, Vector[EdgeDataType]](size, Gen.containerOfN[Vector, EdgeDataType](size, arbitrary[EdgeDataType]))
    } yield matrix

  property("DenseTopology") =
    forAll(denseDirectedTopology(8)) {
      (t: Vector[Vector[EdgeDataType]]) =>
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
          }: _*) &&
          (t.zipWithIndex.map {
            case (row, i) =>
              row.zipWithIndex.map { case (e, j) => (i, j, e) }
          }.flatten.forall { case (i, j, e) => topo.iteredges.contains((i, j, e)) } :| "all test edges in iteredges") &&
          (topo.iteredges.forall {
            case (i, j, e) => (t(i)(j) == e)
          } :| s"all iteredges are in test edges")
    }

  property("DirectedEdges") =
    forAll(sparseDirectedTopology) {
      (t: Vector[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Vector[EdgeDataType]) =>
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

  val sparseUndirectedTopology: Gen[Vector[(Int, Int)]] = Gen.sized { size =>
    for {
      nodes <- Gen.choose(0, size)
      probaLink <- Gen.choose(0.0, 1.0)
      randomFilter <- Gen.containerOfN[Vector, Double](nodes * (nodes - 1) / 2, Gen.choose(0.0, 1.0))
      randomSwitch <- Gen.containerOfN[Vector, Double](nodes * (nodes - 1) / 2, Gen.choose(0.0, 1.0))
    } yield (0 until nodes)
      .map { i => (i + 1 until nodes).map { (i, _) } }.flatten.toVector
      .zip(randomFilter zip randomSwitch)
      .filter { case (_, (rf, _)) => rf < probaLink }
      .map { case ((n1, n2), (_, rs)) => if (rs < 0.5) (n1, n2) else (n2, n1) }
  }

  property("UndirectedEdges") =
    forAll(sparseUndirectedTopology) {
      (t: Vector[(Int, Int)]) =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](t.size, arbitrary[EdgeDataType])) {
          (s: Vector[EdgeDataType]) =>
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

  property("Network") =
    forAll(Gen.containerOf[Vector, NodeDataType](arbitrary[NodeDataType])) { nodes =>
      forAll(sparseDirectedTopology(nodes.length)) { edges =>
        forAll(Gen.containerOfN[Vector, EdgeDataType](edges.size, arbitrary[EdgeDataType])) { edgesData =>
          {
            val testedges = (edges zip edgesData) map { case ((n1, n2), e) => (n1, n2, e) }
            val network = Network.directedSparse[NodeDataType, EdgeDataType](nodes, testedges)
            (testedges.forall { case (n1, n2, e) => network.edge(n1, n2) == Some(e) } :| "all testedges in network") &&
              (network.iteredges.forall { case (n1, n2, e) => testedges.contains((n1, n2, e)) } :| "all network edges in testedges") &&
              (nodes.zipWithIndex.forall { case (ndata, n) => network.node(n) == ndata } :| "nodes index and data respected")
          }
        }
      }
    }
}

