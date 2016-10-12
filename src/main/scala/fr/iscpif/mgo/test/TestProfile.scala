/*
 * Copyright (C) 08/01/13 Guillaume Chérel, Romain Reuillon
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

object SphereProfile extends App {

  import algorithm.profile._
  import util.Random

  //Profile the first dimension of the genome
  val algo = Profile(
    lambda = 100,
    fitness = sphere.compute,
    niche = genomeProfile(x = 0, nX = 10),
    genomeSize = 10)

  val (finalState, finalPopulation) =
    run(algo).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new Random(42))

  println(result(finalPopulation, sphere.scale).mkString("\n"))

}

//
//object StochasticSphereProfile extends App {
//
//  import noisyprofile._
//
//  val muByNiche = 20
//  val lambda = 100
//  def dimensions = 5
//  val maxIteration = 1000
//  val operatorExploration = 0.1
//  val cloneProbability = 0.2
//  val historySize = 100
//
//  def express: (Random, Vector[Double]) => Double = { case (rg, v) => sphere(v) + rg.nextGaussian() * 0.5 + math.sqrt(sphere(v)) }
//  def aggregation(history: Vector[Double]) = history.sum / history.size
//
//  //Niche over the first dimension of the genome
//  def niche = genomeProfile[Individual](
//    values = (Individual.genome composeLens vectorValues).get,
//    x = 0,
//    nX = 10)
//
//  val algo = NoisyProfile(
//    muByNiche = muByNiche,
//    lambda = lambda,
//    fitness = express,
//    aggregation = aggregation,
//    niche = niche,
//    genomeSize = dimensions,
//    historySize = historySize,
//    operatorExploration = operatorExploration,
//    cloneProbability = cloneProbability)
//
//  val ea =
//    runEAUntilStackless[Unit, Individual](
//      stopCondition = afterGeneration[EvolutionState[Unit, ?], Individual](maxIteration),
//      stepFunction = algo.step
//    /*for {
//          individuals <- ka
//          _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) =>
//            individuals.map {
//              i: Individual => state.generation.toString ++ "\t" ++ iValues.get(i).mkString("\t") ++ "\t" ++ (iHistory.get(i).sum / iHistory.get(i).size).toString ++ "\t" ++ iHistory.get(i).length.toString
//            }.mkString("\n")
//          }
//          res <- algo.step
//        } yield res,*/
//    )
//
//  val evolution: EvolutionState[Unit, Vector[Individual]] =
//    for {
//      gs <- algo.initialGenomes
//      gsRNG <- zipWithRandom[EvolutionState[Unit, ?], Genome](gs)
//      initialPopEval = gsRNG.map { case (rg, g) => buildIndividual(g, express(rg, vectorValues.get(g))) }
//      // _ <- writeS { (state: EvolutionData[Unit], individuals: Vector[Individual]) => "generation\t" ++ Vector.tabulate(dimensions)(i => s"g$i").mkString("\t") ++ "\t" ++ "fitness" ++ "\t" ++ "historyLength" }.run(Vector.empty)
//      finalpop <- ea.run(initialPopEval)
//    } yield finalpop
//
//  val (finalstate, finalpop) = algo.run(evolution, new Random(42))
//
//  println("---- Fitnesses ----")
//  println(
//    profile(finalpop, niche).map {
//      i =>
//        Vector(
//          i.historyAge,
//          (Individual.genome composeLens Genome.values).get(i)(0),
//          aggregation(vectorFitness.get(i)),
//          vectorFitness.get(i).size
//        ).mkString(",")
//    }.mkString("\n"))
//
//}
