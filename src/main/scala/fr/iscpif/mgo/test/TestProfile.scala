///*
// * Copyright (C) 08/01/13 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Affero General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.test
//
//import fr.iscpif.mgo.algorithm._
//import fr.iscpif.mgo._
//import scalaz._
//import util.Random
//
//object Profile {
//  val profile = new Profile {
//    def mu = 100
//    val fitness = Fitness(_.phenotype)
//    type P = Double
//    val niche = genomeProfile(0, 100)
//  }
//}
//
//object TestProfileSphere extends App {
//
//  import Profile.profile
//  import profile._
//
//  def dimensions = 10
//  def problem(g: G) = State { rng: Random => (rng, sphere(genomeValues.get(g))) }
//
//  val evo = evolution(profile, 100)(randomGenome(dimensions), problem, afterGeneration(100))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}
//
//object TestProfileRastrigin extends App {
//
//  import Profile.profile
//  import profile._
//
//  def dimensions = 10
//  def problem(g: G) = State { rng: Random => (rng, rastrigin(genomeValues.get(g))) }
//
//  val evo = evolution(profile, 100)(randomGenome(dimensions), problem, afterGeneration(100))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}