/*
 * Copyright (C) 2012 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif

package object mgo {
  
  implicit def traversable2Population[G, I](seq: Traversable[PopulationElement[G, I]]) =
    new Population[G, I] {
      override val content = seq.toIndexedSeq
    }
  
  implicit def population2IndexedSeq[G, I](pop: Population[G, I]) = pop.content

}