/*
 * Copyright (C) 2015 Guillaume Chérel
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

package mgo.tools.network

/**
 * N = Node data type
 * E = Edge data type
 */
trait Network[N, E] {
  def node(u: Int): N = nodes(u)
  def iternodes: Iterator[(Int, N)] = nodes.indices.iterator zip nodes.iterator

  def nodes: Vector[N]
  def edge(u: Int, v: Int): Option[E]
  def iteredges: Iterator[(Int, Int, E)]

  /** Either digraph or graph */
  def dotGraphType: String

  def toDot(graphId: String,
    nodeAttr: N => Seq[(String, String)],
    edgeAttr: E => Seq[(String, String)],
    additionalStatements: String): String =
    s"""$dotGraphType $graphId {
       |${additionalStatements.lines.map { "  " ++ _ }.mkString { "\n" }}
       |${toDotNodes(nodeAttr).lines.map { "  " ++ _ }.mkString { "\n" }}
       |${toDotEdges(edgeAttr).lines.map { "  " ++ _ }.mkString { "\n" }}
       |}""".stripMargin

  def toDotNodes(nodeAttr: N => Seq[(String, String)]): String =
    iternodes.map {
      case (i, n) => s"""$i [ ${
        nodeAttr(n).map {
          case (k, v) => s"$k = $v"
        }.mkString(", ")
      } ]"""
    }.mkString("\n")

  /** Either -> or -- */
  def dotEdgeOperator: String

  def toDotEdges(edgeAttr: E => Seq[(String, String)]): String =
    iteredges.map {
      case (u, v, e) => s"""$u $dotEdgeOperator $v [ ${
        edgeAttr(e).map {
          case (k, v) => s"$k = $v"
        }.mkString(", ")
      } ]"""
    }.mkString("\n")

  def toJSONNodeLink: String =
    s"""{
  "nodes":${toJSONNodes.lines.map { "  " ++ _ }.mkString { "\n" }},
  "links":${toJSONLinks.lines.map { "  " ++ _ }.mkString { "\n" }}
}"""

  def toJSONNodes: String =
    "[\n" ++
      iternodes.map { case (i, n) => s"""  {"id":$i, "data":"$n"}""" }.mkString(",\n") ++
      "\n]"

  def toJSONLinks: String =
    "[\n" ++
      iteredges.map { case (u, v, e) => s"""  {"source":$u, "target": $v, "data": $e}""" }.mkString(",\n") ++
      "\n]"

}

object Network {
  def directedSparse[N, E](
    _nodes: IndexedSeq[N],
    _edges: Traversable[(Int, Int, E)]): Network[N, E] with DirectedEdges[E] with SparseTopology[E] =
    new Network[N, E] with DirectedEdges[E] with SparseTopology[E] {
      val nodes = _nodes.toVector
      val mapin = SparseTopology.mapinFrom(_edges)
      val mapout = SparseTopology.mapoutFrom(_edges)
    }

  def directedSparse[E](
    nbOfNodes: Int,
    _edges: Traversable[(Int, Int, E)]): Network[Unit, E] with DirectedEdges[E] with SparseTopology[E] =
    new Network[Unit, E] with DirectedEdges[E] with SparseTopology[E] {
      val nodes: Vector[Unit] = Vector.fill(nbOfNodes)((): Unit)
      val mapin = SparseTopology.mapinFrom(_edges)
      val mapout = SparseTopology.mapoutFrom(_edges)
    }

  def directedDense[N, E](
    _nodes: IndexedSeq[N],
    _edges: Vector[Vector[E]]): Network[N, E] with DirectedEdges[E] with DenseTopology[E] =
    new Network[N, E] with DirectedEdges[E] with DenseTopology[E] {
      val nodes = _nodes.toVector
      val matrix = _edges
    }
}

