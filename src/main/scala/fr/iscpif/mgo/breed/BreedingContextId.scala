/*
 * Copyright (C) 06/08/2015 Guillaume Chérel
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
package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.InitialGenome

import scala.util.Random
import scalaz._
import Scalaz._

import scala.language.higherKinds

trait BreedingContextId <: BreedingContext {

  type BreedingContext[A] = Id[A]
  override implicit lazy val monadBreedingContext: Monad[BreedingContext] = implicitly[Monad[Id]]
  /*override implicit def monadBreedingContext: Monad[BreedingContext] = new Monad[BreedingContext] {
    def bind[A, B](fa: BreedingContext[A])(f: (A) ⇒ BreedingContext[B]): BreedingContext[B] = implicitly[Monad[Id]].bind[A, B](fa)(f)

    def point[A](a: ⇒ A): BreedingContext[A] = implicitly[Monad[Id]].point[A](a)
  }*/

  /** extract the content from the breeding monad */
  def unwrapBreedingContext[Z](x: BreedingContext[Z], population: Population[G, P, F], archive: A): Z = x: Id[Z]

  //  override def initialGenome(implicit rng: Random): BreedingContext[G] = {
  //    val rg = initialGenome
  //    rg.point[BreedingContext]
  //  }
  //
  //  def cloneInContext(c: Individual[G, P, F]): BreedingContext[G] = clone(c).point[BreedingContext]

}
