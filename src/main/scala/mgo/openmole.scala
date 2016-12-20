/**
 * Created by Romain Reuillon on 07/01/16.
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
 *
 */

package mgo

import cats._
import contexts._
import breeding._
import elitism._
import squants._

object openmole {

  trait Integration[A, V, P] {
    type M[_]
    type I
    type G
    type S

    implicit def iManifest: Manifest[I]
    implicit def gManifest: Manifest[G]
    implicit def sManifest: Manifest[S]

    implicit def mMonad: Monad[M]
    implicit def mGeneration: Generation[M]
    implicit def mStartTime: StartTime[M]

    def operations(a: A): Ops

    trait Ops {
      def initialState(rng: util.Random): S
      def initialGenomes(n: Int): M[Vector[G]]
      def buildIndividual(genome: G, phenotype: P): I
      def values(genome: G): V
      def genome(individual: I): G
      def phenotype(individual: I): P
      def genomeValues(individual: I) = values(genome(individual))
      def randomLens: monocle.Lens[S, util.Random]
      def startTimeLens: monocle.Lens[S, Long]
      def generation(s: S): Long
      def breeding(n: Int): Breeding[M, I, G]
      def elitism: Elitism[M, I]
      def migrateToIsland(i: Vector[I]): Vector[I]
      def migrateFromIsland(population: Vector[I]): Vector[I]
    }

    def run[T](m: M[T], s: S): (T, S)

    def afterGeneration(g: Long) = mgo.afterGeneration[M, I](g)
    def afterDuration(d: Time) = mgo.afterDuration[M, I](d)
  }

  trait Stochastic { self: Integration[_, _, _] =>
    def samples(s: I): Long
  }

  trait Profile[A] { self: openmole.Integration[A, _, _] =>
    def profile(a: A)(population: Vector[I]): Vector[I]
  }
}
