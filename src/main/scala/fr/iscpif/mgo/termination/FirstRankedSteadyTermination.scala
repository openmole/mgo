/*
 * Copyright (C) 2011 srey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.termination

import fr.iscpif.mgo._
import fr.iscpif.mgo.ranking.Rank
import fr.iscpif.mgo.ranking.Rank._
import fr.iscpif.mgo.selection._
import fr.iscpif.mgo.tools.Math

trait FirstRankedSteadyTermination extends Termination {
  self: Evolution { type I <: Individual[_] with Rank} =>
  
  type STATE = Int
  
  def initialState = 0
  
  def steadySince: Int

  def terminated(a1: IndexedSeq[I], a2: IndexedSeq[I], step: STATE): (Boolean, STATE) = {
    val newStep = if ( Math.allTheSame(firstRanked(a1).map {_.fitness.values},firstRanked(a2).map {_.fitness.values})) step + 1
    else  0
    (newStep >= steadySince, newStep)
  }

  def sameFirstRanked(
    a1: IndexedSeq[I],
    a2: IndexedSeq[I]) =
      Math.allTheSame (
      firstRanked(a1).map{_.fitness.values},
      firstRanked(a2).map{_.fitness.values}
    )
 
    
 
  
  
}
