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

package fr.iscpif.mgo

object Population {
  
  def empty[G, I]: Population[G, I] = IndexedSeq.empty
  //def apply(evaluated: IndexedSeq[(G, Fitness)], individuals: IndexedSeq[I]) = 
}

object PopulationElement {
  def apply[G, MF](i: Individual[G], mf: MF) = 
    new PopulationElement[G, MF](i.genome, i.fitness, mf)
}

trait Population[+G, +MF] {

  def content: IndexedSeq[PopulationElement[G, MF]]
  def individuals: IndexedSeq[Individual[G]] = content map { _.toIndividual }
  
  override def toString = content.toString  
}

case class PopulationElement[+G, +MF](val genome: G, val fitness: Fitness, val metaFitness: MF) {
  def toIndividual = Individual(genome, fitness)
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", metaFitness = " + metaFitness + ")"
}