///*
// * Copyright (C) Guillaume Chérel 06/05/14
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.test
//
//import fr.iscpif.mgo._
//import fr.iscpif.mgo.algorithm._
//
//import Contexts.default._
//import Contexts._
//import Expressions._
//
//import scala.util.Random
//import scalaz._
//import Scalaz._
//
//object ZDT4PSE extends App {
//  import PSE.Algorithm._
//
//  val maxiter: Int = 100
//  val initialPopSize: Int = 10
//  val mu: Int = 10
//  val lambda: Int = 10
//  val genomeSize: Int = 10
//  val operatorExploration: Double = 0.1
//  val cloneProbability = 0.1
//  val anchor: Vector[Double] = Vector(0.0, 0.0)
//  val discretisationStep: Vector[Double] = Vector(0.1, 20)
//  val lowBound: Vector[Double] = Vector(0.0, 0.0)
//  val highBound: Vector[Double] = Vector(1.0, 200.0)
//
//  val express: Vector[Double] => Vector[Double] = zdt4(_).toVector
//
//  val algo = PSE.Algorithm(
//    initialPopSize = initialPopSize,
//    keepPerCell = mu,
//    lambda = lambda,
//    express = express,
//    genomeSize = genomeSize,
//    operatorExploration = operatorExploration,
//    cloneProbability = cloneProbability,
//    anchor = anchor,
//    discretisationStep = discretisationStep,
//    lowBound = lowBound,
//    highBound = highBound)
//
//  def k[A] = Kleisli.kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], A] _
//  def ka = Kleisli.ask[EvolutionStateMonad[HitMap]#l, Vector[Individual]]
//
//  val ea: Kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Vector[Individual]] =
//    runEAUntilStackless[HitMap, Individual](
//      stopCondition = Kleisli.kleisli[EvolutionStateMonad[HitMap]#l, Vector[Individual], Boolean]({ (individuals: Vector[Individual]) =>
//        evolutionStateGenerational[HitMap].generationReached(maxiter)
//      }),
//      stepFunction =
//        for {
//          individuals <- ka
//          hitmap <- k[HitMap] { _ => mHitMap.get }
//          _ <- writeS((state: EvolutionData[HitMap], individuals: Vector[Individual]) =>
//            s"Generation = ${state.generation}, Volume discovered = ${state.s.values.count { _ > 0 }.toString}")
//          _ <- writeS((state: EvolutionData[HitMap], individuals: Vector[Individual]) =>
//            individuals.map {
//              i: Individual => state.generation.toString ++ "\t" ++ (iGenome >=> gValues).get(i).mkString("\t") ++ "\t" ++ iPattern.get(i).mkString("\t")
//            }.mkString("\n"))
//          res <- algo.step
//        } yield res,
//      start = EvolutionData[HitMap](random = newRNG(1), s = Map.empty[Vector[Int], Int])
//    )
//
//  val evolution: EvolutionState[HitMap, Vector[Individual]] =
//    for {
//      ig <- algo.initialGenomes
//      initialPop = ig.map { g => Individual(g, express(gValues.get(g))) }
//      _ <- writeS { (state: EvolutionData[HitMap], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(genomeSize)(i => s"g$i").mkString("\t") ++ "\t" ++ Vector.tabulate(individuals.head.pattern.size)(i => s"p$i").mkString("\t") }.run(initialPop)
//      finalpop <- ea.run(initialPop)
//    } yield finalpop
//
//  val (finalstate, finalpop) = algo.unwrap[Vector[Individual]](evolution)
//
//  println("---- Final State ----")
//  println(finalstate)
//
//  /*println("---- Final Population ----")
//  println(finalpop.mkString("\n"))
//  */
//
//  println("---- Patterns ----")
//  println(finalpop.map { (_: Individual).pattern }.sortWith {
//    case (a, b) =>
//      val i = (a.zipAll(b, Double.MinValue, Double.MinValue).indexWhere { case (x1, x2) => x1 != x2 })
//      if (i == -1) false
//      else a(i) < b(i)
//  }.mkString("\n"))
//
//  println("---- Cells in HitMap ----")
//  println(finalstate.s.toVector.sortWith {
//    case (a, b) =>
//      val i = (a._1.zipAll(b._1, Int.MinValue, Int.MinValue).indexWhere { case (x1, x2) => x1 != x2 })
//      if (i == -1) false
//      else a._1(i) < b._1(i)
//  }.mkString("\n"))
//}
