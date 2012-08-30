/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo



object Individual {

  implicit def individual2Fitness(i: Individual[_]) = i.fitness
  
  def apply[G](g: G, e: G => Fitness) = 
    new Individual[G] {
      val genome = g
      val fitness = e(g)
    }

  def apply[G](g: G, f: Fitness) = 
    new Individual[G] {
      val genome = g
      val fitness = f
    }
  
}

/**
 * An individual of the evolution
 */
trait Individual[+G] {
  /// The genome of this individual
  def genome: G
  
  /// The fitness evaluated for the genome
  def fitness: Fitness
  
  def toTuple = genome -> fitness
  override def toString = "( genome = " + genome.toString + ", fitness = " + fitness.toString + ")"
}
