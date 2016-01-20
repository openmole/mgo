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

package fr.iscpif.mgo

import fr.iscpif.mgo.breeding._
import fr.iscpif.mgo.contexts.{ StartTime, Generational }
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.niche._

import scala.concurrent.duration.Duration
import scala.util.Random
import scalaz._
import Scalaz._

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
    implicit def mGenerational: Generational[M]
    implicit def mStartTime: StartTime[M]

    def operations(a: A): Ops

    trait Ops {
      def initialState(rng: Random): S
      def initialGenomes(n: Int): M[Vector[G]]
      def buildIndividual(genome: G, phenotype: P): I
      def values(genome: G): V
      def genome(individual: I): G
      def phenotype(individual: I): P
      def genomeValues(individual: I) = values(genome(individual))
      def randomLens: monocle.Lens[S, Random]
      def startTimeLens: monocle.Lens[S, Long]
      def generation(s: S): Long
      def breeding(n: Int): Breeding[M, I, G]
      def elitism: Elitism[M, I]
      def migrateToIsland(i: I): I
      def migrateFromIsland(i: I): I
    }

    def unwrap[T](m: M[T], s: S): (S, T)
    def run[A, B](start: S, action: => M[B]): (S, B) = unwrap(action, start)

    def afterGeneration(g: Long) = stop.afterGeneration[M, I](g)
    def afterDuration(d: Duration) = stop.afterDuration[M, I](d)
  }

  trait Stochastic { self: Integration[_, _, _] =>
    def samples(s: I): Long
  }

  trait Profile[A] { self: openmole.Integration[A, _, _] =>
    def profile(a: A)(population: Vector[I]): Vector[I]
  }
}
