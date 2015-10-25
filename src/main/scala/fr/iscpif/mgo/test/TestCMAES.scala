///*
// * Copyright (C) 2014 Romain Reuillon
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
//
//import scala.util.Random
//import scalax.io
//
//object TestCMAES extends App {
//
//  val m = new Rastrigin with CMAES with CounterTermination {
//    /** Number of steps before the algorithm stops */
//    override def steps: Int = 1000
//    override def genomeSize: Int = 5
//
//    def lambda = 200
//
//    //To be fair with other methods
//    override def guess(implicit rng: Random) = Seq.fill[Double](genomeSize)(rng.nextDouble)
//  }
//
//  implicit val rng = new Random(46)
//
//  val res =
//    m.evolve.untilConverged {
//      s =>
//        println(s.generation + " " + s.population.size + " " + s.population.toIndividuals.map(i => m.aggregate(m.fitness(i))).min)
//    }.population
//
//  val output = io.Resource.fromFile("/tmp/res.csv")
//  for {
//    r <- res.toIndividuals
//  } {
//    def line = m.scale(m.values.get(r.genome)) ++ m.fitness(r)
//    output.append(line.mkString(",") + "\n")
//  }
//
//}
