package fr.iscpif.mgo.termination

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


import fr.iscpif.mgo._

trait CounterTermination extends Termination {
  self: Evolution =>
  
   type STATE = Int
  
  def initialState = 0
  
  def maxStep: Int
  def terminated(oldPop: Population[G, MF], newPop: Population[G, MF], step: STATE) = 
    (step >= maxStep, step + 1)
}
