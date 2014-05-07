/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import genome.G
import scala.util.Random
import fr.iscpif.mgo.modifier.MF

/**
 * Cake layer to eliminated elements of a population
 */
trait Elitism extends G with P with F with MF with Modifier {

  def elitism(oldGeneration: Seq[Individual[G, P, F]], newGeneration: Seq[Individual[G, P, F]], archive: A)(implicit aprng: Random): Seq[Individual[G, P, F]]

}
