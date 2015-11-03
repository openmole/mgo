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

trait CloneFunctions { this: Algorithm =>

  def interleaveClones(genomes: State[AlgorithmState, List[G]], clones: State[AlgorithmState, G], ratio: Double, lambda: Int)(implicit random: monocle.Lens[AlgorithmState, Random]): State[AlgorithmState, List[G]] = {

    @tailrec def interleaveClones0(acc: List[G], pool: List[G], lambda: Int, state: AlgorithmState): (AlgorithmState, List[G]) = {
      if (lambda <= 0) (state, acc)
      else if (random.get(state).nextDouble() < ratio) {
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

    State { state: AlgorithmState => interleaveClones0(List(), List(), lambda, state) }
  }

  def mergeClones(population: Pop)(implicit genomeEquality: Equal[G], merge: MergePhenotype) = {
    def newPop =
      group(population.toList)(genomeEquality.contramap[Ind](_.genome)).
        map {
          _.reduce {
            (i1, i2) =>
              if (i1.born < i2.born) i1.copy(phenotype = merge.append(i1.phenotype, i2.phenotype))
              else i2.copy(phenotype = merge.append(i2.phenotype, i1.phenotype))
          }
        }

    State { s: AlgorithmState => s -> newPop.toVector }
  }

  trait MergePhenotype {
    def append(old: P, young: => P): P
  }

  def youngest = new MergePhenotype {
    override def append(old: P, young: => P): P = young
  }

  sealed trait Age
  case class History[+C](history: List[C], age: Int @@ Age = 0)

  implicit def historyHistoryLens[C] = monocle.macros.Lenser[History[C]](_.history)
  implicit def historyAgeLens[C] = monocle.macros.Lenser[History[C]](_.age)
  implicit def historyToList[C](h: History[C]) = h.history
  implicit def stateOfCToHistory[S, C](c: State[S, C]) = c.map { c => History(List(c)) }

  def queue[C](size: Int)(implicit historyLens: monocle.Lens[P, List[C]], ageLens: monocle.Lens[P, Int @@ Age]) = new MergePhenotype {
    override def append(old: P, young: => P): P = {
      val oldAge = ageLens.get(old)
      val youngAge = ageLens.get(young)

      def oldP = historyLens.get(old).takeRight(oldAge)
      def youngP = historyLens.get(young).takeRight(youngAge)

      def newP = historyLens.set((oldP ::: youngP).takeRight(size))(old)
      ageLens.set(oldAge + youngAge)(newP)
    }
  }

}
