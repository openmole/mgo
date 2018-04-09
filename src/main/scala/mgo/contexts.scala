package mgo

import freedsl.dsl._
import freedsl.io.IOInterpreter
import freedsl.random.RandomInterpreter
import freedsl.system.SystemInterpreter
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

  @tagless trait HitMap {
    def get(): FS[Map[Vector[Int], Int]]
    def set(map: Map[Vector[Int], Int]): FS[Unit]
  }

  case class HitMapInterpreter(var map: Map[Vector[Int], Int]) extends HitMap.Handler[Evaluated] {
    def get() = result(map)
    def set(m: Map[Vector[Int], Int]) = result(map = m)
  }

  //  implicit def convert[M[_]](implicit vhm: VectorHitMap[M]) = new HitMap[M, Vector[Int]] {
  //    def get() = vhm.get()
  //    def set(map: Map[Vector[Int], Int]) = vhm.set(map)
  //  }

  trait Archive[M[_], I] {
    def put(i: Seq[I]): M[Unit]
    def get(): M[Vector[I]]
  }

  @tagless trait ReachMap {
    def reached(c: Vector[Int]): FS[Boolean]
    def setReached(c: Seq[Vector[Int]]): FS[Unit]
    def get(): FS[Seq[Vector[Int]]]
  }

  case class ReachMapInterpreter(map: collection.mutable.HashSet[Vector[Int]]) extends ReachMap.Handler[Evaluated] {
    def reached(c: Vector[Int]) = result(map.contains(c))
    def setReached(c: Seq[Vector[Int]]) = result(map ++= c)
    def get() = result(map.toVector)
  }

  type IO[T[_]] = freedsl.io.IO[T]
  type Random[T[_]] = freedsl.random.Random[T]

  object run {
    object Implicits {
      def apply[S](state: EvolutionState[S]): Implicits =
        Implicits()(GenerationInterpreter(state.generation), RandomInterpreter(state.random), StartTimeInterpreter(state.startTime), IOInterpreter(), SystemInterpreter())

    }
    case class Implicits(implicit generationInterpreter: GenerationInterpreter, randomInterpreter: RandomInterpreter, startTimeInterpreter: StartTimeInterpreter, iOInterpreter: IOInterpreter, systemInterpreter: SystemInterpreter)

    def apply[T](rng: util.Random)(f: Implicits => T): T = {
      val state = EvolutionState[Unit](random = rng, s = ())
      apply(state)(f)
    }

    def apply[T, S](state: EvolutionState[S])(f: Implicits => T): T = f(Implicits(state))
  }

}
