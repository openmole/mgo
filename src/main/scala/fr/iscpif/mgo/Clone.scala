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

import scala.annotation.tailrec
import scala.util.Random
import scalaz._

object clone {
  sealed trait Age

  def interleaveClones[G, P, S](
    genomes: State[AlgorithmState[S], Vector[G]],
    clones: State[AlgorithmState[S], G],
    lambda: Int,
    cloneStrategy: CloneStrategy[P]): State[AlgorithmState[S], Vector[G]] = {

    @tailrec def interleaveClones0(acc: List[G], pool: List[G], lambda: Int, state: AlgorithmState[S]): (AlgorithmState[S], List[G]) = {
      if (lambda <= 0) (state, acc)
      else if (random.get(state).nextDouble() < cloneStrategy.cloneRate) {
        val (newState, c) = clones.run(state)
        interleaveClones0(c :: acc, pool, lambda - 1, newState)
      } else {
        pool match {
          case Nil =>
            val (newState, gs) = genomes.run(state)
            if (gs.isEmpty) (newState, acc)
            else interleaveClones0(gs.head :: acc, gs.toList.tail, lambda - 1, newState)
          case h :: tail =>
            interleaveClones0(h :: acc, tail, lambda - 1, state)
        }
      }
    }

    State { state: AlgorithmState[S] => interleaveClones0(List(), List(), lambda, state) }.map(_.toVector)
  }

  def applyCloneStrategy[G, P](population: Population[Individual[G, P]], cloneStrategy: CloneStrategy[P])(implicit genomeEquality: Equal[G]) = {
    def newPop =
      group(population.toList)(genomeEquality.contramap[Individual[G, P]](_.genome)).
        map {
          _.reduce {
            (i1, i2) =>
              val (old, young) = if (i1.born < i2.born) (i1, i2) else (i2, i1)
              old.copy(phenotype = cloneStrategy.append(old.phenotype, young.phenotype))
          }
        }

    newPop.toVector
  }

  trait CloneStrategy[P] {
    def append(old: P, young: => P): P
    def cloneRate: Double
  }

  def youngest[P] = new CloneStrategy[P] {
    override def append(old: P, young: => P): P = young
    def cloneRate = 0.0
  }

  case class History[C](history: List[C], age: Int @@ Age = 1)

  implicit def historyToList[C](h: History[C]) = h.history
  implicit def stateOfCToHistory[S, C](c: State[S, C]) = c.map { c => History(List(c)) }

  def queue[C](size: Int, cloneRate: Double) = {
    val _cloneRate = cloneRate
    new CloneStrategy[History[C]] {
      def cloneRate = _cloneRate

      override def append(old: History[C], young: => History[C]): History[C] = {
        val oldAge = old.age
        val youngAge = young.age

        def oldP = old.history.takeRight(oldAge)
        def youngP = young.history.takeRight(youngAge)

        def newP = old.copy(history = (oldP ::: youngP).takeRight(size))

        newP.copy(age = oldAge + youngAge)
      }
    }
  }

}
