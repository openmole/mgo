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

package fr.iscpif.mgo.genome

import fr.iscpif.mgo._
import monocle._
import scala.util.Random

/**
 * Base of the cake for a genetic algorithm evolutions (algorithms evolving
 * sequences of doubles).
 */
trait GA extends G with RandomGenome with GenomeClamping {
  /** The value part of the genome actually used for the optimisation */
  def values: Lens[G, Seq[Double]] = clamp(rawValues)
  def rawValues: Lens[G, Seq[Double]]

  /** Size of the value part of the genome */
  def genomeSize: Int

  def randomGenome(implicit rng: Random): G

  override def genomesEqualOn(g: G): Any = values.get(g)
}
