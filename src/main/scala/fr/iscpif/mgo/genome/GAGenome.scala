///*
// * Copyright (C) 2012 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
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
//package fr.iscpif.mgo.genome
//
//import monocle.macros.Lenses
//
//import scala.util.Random
//
//import scalaz.Equal
//import collection.immutable.Stream
//
//object GAGenome {
//  implicit def gaEquality = new Equal[GAGenome] {
//    override def equal(a1: GAGenome, a2: GAGenome): Boolean =
//      a1.values.value.toSeq == a2.values.value.toSeq
//  }
//
//  implicit def randomGenome(implicit size: GenomeSize[GAGenome], rng: Random) = new RandomGenome[GAGenome] {
//    def apply() =
//      new GAGenome(GenomeValue(Stream.continually(rng.nextDouble).take(size.value).toArray))
//  }
//}
//
//// Clamping ?
//@Lenses class GAGenome(
//  values: GenomeValue[Array[Double]],
//  mutation: Option[Int] = None,
//  crossover: Option[Int] = None)
//
