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

package fr.iscpif.mgo

import scala.language.higherKinds
import scalaz._


trait Mutation <: Pop { this: Algorithm =>
  type Mutation = (G => State[AlgorithmState, G])
}

trait MutationDefault <: Mutation with Genome { this: Algorithm =>

  def gaussianMutation(sigma: Double)(implicit values: monocle.Lens[G, GenomeValue[Seq[Double]]]) = new Mutation {
    override def apply(g: G) = State { state: AlgorithmState =>
       val newValues = values.modify(g => GenomeValue(g.value.map(_ + (state.random.nextGaussian * sigma))))(g)
      (state, newValues)
    }
  }

}