/*
 * Copyright (C) 20/11/12 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import util.Random
import java.io._

object TestMap extends App {

  val m =
    new Rastrigin with MG with MapArchive with MapModifier with GASigmaFactory with MaxAggregation with SBXBoundedCrossover with CrowdingDiversity with MapElitism with CoEvolvingSigmaValuesMutation with BinaryTournamentSelection with ParetoRanking with CounterTermination with StrictDominance with MapGenomePlotter with GeneticBreeding {
      def genomeSize: Int = 6
      def lambda: Int = 200
      def neighbours = 8
      def steps = 500
      def x: Int = 0
      def y: Int = 1
      def nX: Int = 100
      def nY: Int = 100
    }

  implicit val rng = new Random

  val res = m.evolve.untilConverged(s => println(s.generation)).individuals

  val writer = new FileWriter(new File("/tmp/matrix.csv"))
  for {
    i <- res
    (x, y) = m.plot(i)
    v = m.aggregate(i.fitness)
    if !v.isPosInfinity
  } writer.write("" + x + "," + y + "," + v + "\n")
  writer.close

}