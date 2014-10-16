/*
 * Copyright (C) 2014 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.dynamic

import monocle._
import fr.iscpif.mgo._
import scala.util.Random

trait DynamicApplicationGAGenome <: GA with Sigma with F {

  type Ancestors = (F, F)

  case class Genome(
    values: Seq[Double],
    sigma: Seq[Double],
    ancestors: Option[Ancestors] = None,
    mutation: Option[Int] = None,
    crossover: Option[Int] = None)

  type G = Genome

  def rawValues = Lenser[G](_.values)
  def sigma = Lenser[G](_.sigma)
  def ancestors = Lenser[G](_.ancestors)
  def mutation = Lenser[G](_.mutation)
  def crossover = Lenser[G](_.crossover)

  def randomGenome(implicit rng: Random) = {
    def rnd = Stream.continually(rng.nextDouble).take(genomeSize).toIndexedSeq
    Genome(rnd, rnd)
  }

}
