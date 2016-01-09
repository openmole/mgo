/*
 * Copyright (C) Guillaume Chérel 06/05/14
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import fr.iscpif.mgo.algorithm._
import contexts.default._
import stop._

import scala.util.Random
import scalaz._
import Scalaz._
import pse._

object ZDT4PSE extends App {

  val maxIterations: Int = 100
  val lambda: Int = 10
  val genomeSize: Int = 10
  val operatorExploration: Double = 0.1
  val cloneProbability = 0.1
  val lowBound: Vector[Double] = Vector(0.0, 0.0)
  val highBound: Vector[Double] = Vector(1.0, 200.0)
  val definition: Vector[Int] = Vector(10, 10)

  def express = zdt4 _
  def pattern = patternGrid(lowBound, highBound, definition) _

  val algo = PSE(
    lambda = lambda,
    phenotype = express,
    pattern = pattern,
    genomeSize = genomeSize,
    operatorExploration = operatorExploration)

  val ea =
    runEAUntilStackless[HitMap, Individual](
      stopCondition = afterGeneration[EvolutionState[HitMap, ?], Individual](maxIterations),
      stepFunction = algo.step
    /*for {
          individuals <- ka
          hitmap <- k[HitMap] { _ => mHitMap.get }
          _ <- writeS((state: EvolutionData[HitMap], individuals: Vector[Individual]) =>
            s"Generation = ${state.generation}, Volume discovered = ${state.s.values.count { _ > 0 }.toString}")
          _ <- writeS((state: EvolutionData[HitMap], individuals: Vector[Individual]) =>
            individuals.map {
              i: Individual => state.generation.toString ++ "\t" ++ (iGenome >=> gValues).get(i).mkString("\t") ++ "\t" ++ iPattern.get(i).mkString("\t")
            }.mkString("\n"))
          res <- algo.step
        } yield res*/
    )

  val evolution: EvolutionState[HitMap, Vector[Individual]] =
    for {
      ig <- algo.initialGenomes
      initialPop = ig.map { g => buildIndividual(g, express(Genome.values.get(g))) }
      //_ <- writeS { (state: EvolutionData[HitMap], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(genomeSize)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(individuals.head.pattern.size)(i => s"p$i").mkString("\t") }.run(initialPop)
      finalpop <- ea.run(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = algo.run(evolution, new Random(42))

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

}
