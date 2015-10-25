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
//package fr.iscpif.mgo.termination
//
//import fr.iscpif.mgo._
//
//import scala.util.Random
//
///**
// * Terminate the algorithm after a given share of time.
// */
//trait TimedTermination extends Termination {
//
//  /** Duration of the algorithm in ms */
//  def duration: Long
//
//  type STATE = Long
//
//  def initialState = System.currentTimeMillis
//
//  override def terminated(population: Population[G, P, F], begin: STATE)(implicit rng: Random) =
//    (begin + duration <= System.currentTimeMillis, begin)
//
//}
