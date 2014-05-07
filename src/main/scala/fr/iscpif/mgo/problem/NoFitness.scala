/*
 * Copyright (C) 12/02/14 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.problem

import fr.iscpif.mgo._
import scala.util.Random
import scalaz.Lens

trait NoFitness <: Problem with MG {

  type F = None.type

  override def fitness: Lens[F, Seq[Double]] =
    Lens.lensu((v, f) => None, v => Seq.empty)

  def evaluate(phenotype: P, rng: Random): F = None
}
