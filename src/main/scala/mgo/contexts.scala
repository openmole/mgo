package mgo

import freedsl.dsl._
import freedsl.io.IOInterpreter
import freedsl.random.RandomInterpreter
import freestyle.tagless._
import mgo.algorithm.EvolutionState

object contexts {

  case class GenerationInterpreter(g: Long) extends Generation.Handler[Evaluated] {
    var generation = g
    def get() = result(generation)
    def increment() = result(generation += 1)
  }

  @tagless trait Generation {
    def get(): FS[Long]
    def increment(): FS[Unit]
  }

  case class StartTimeInterpreter(startTime: Long) extends StartTime.Handler[Evaluated] {
    def get() = result(startTime)
  }

  @tagless trait StartTime {
    def get(): FS[Long]
  }

  trait HitMap[M[_], C] {
    def get(): M[Map[C, Int]]
    def set(map: Map[C, Int]): M[Unit]
  }

  @tagless trait VectorHitMap {
    def get(): FS[Map[Vector[Int], Int]]
    def set(map: Map[Vector[Int], Int]): FS[Unit]
  }

  case class VectorHitMapInterpreter(var map: Map[Vector[Int], Int]) extends VectorHitMap.Handler[Evaluated] {
    def get() = result(map)
    def set(m: Map[Vector[Int], Int]) = result(map = m)
  }

  implicit def convert[M[_]](implicit vhm: VectorHitMap[M]) = new HitMap[M, Vector[Int]] {
    def get() = vhm.get()
    def set(map: Map[Vector[Int], Int]) = vhm.set(map)
  }

  type IO[T[_]] = freedsl.io.IO[T]
  type Random[T[_]] = freedsl.random.Random[T]

  object run {
    object Implicits {
      def apply[S](state: EvolutionState[S]): Implicits =
        Implicits()(GenerationInterpreter(state.generation), RandomInterpreter(state.random), StartTimeInterpreter(state.startTime), IOInterpreter())

    }
    case class Implicits(implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter)

    def apply[T](rng: util.Random)(f: Implicits => T): T = {
      val state = EvolutionState[Unit](random = rng, s = ())
      apply(state)(f)
    }

    def apply[T, S](state: EvolutionState[S])(f: Implicits => T): T = f(Implicits(state))
  }

}
