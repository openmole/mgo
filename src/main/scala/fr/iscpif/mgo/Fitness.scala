/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.mgo

object fitness {

  type Fitness[G, P, F] = (Individual[G, P] => F)

  def Fitness[G, P, F](f: (Individual[G, P] => F)) = new Fitness[G, P, F] {
    override def apply(v1: Individual[G, P]): F = f(v1)
  }

  implicit class IndividualFitnessDecorator[G, P](i: Individual[G, P]) {
    def fitness[F](implicit f: Fitness[G, P, F]): F = f(i)
  }

}
