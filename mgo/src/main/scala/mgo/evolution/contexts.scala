package mgo.evolution

import java.util.UUID

import cats._
import cats.implicits._
import freestyle.tagless._
import mgo.evolution.algorithm.EvolutionState
import squants.Time
import mgo.tagtools._

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

  import freestyle.tagless._

  @tagless trait Random {
    def nextDouble(): FS[Double]
    def nextInt(n: Int): FS[Int]
    def nextBoolean(): FS[Boolean]
    def shuffle[A](s: Vector[A]): FS[Vector[A]]
    def use[T](f: util.Random => T): FS[T]
  }

  object RandomInterpreter {
    def apply(seed: Long): RandomInterpreter = new RandomInterpreter(new util.Random(seed))
    def apply(random: util.Random): RandomInterpreter = new RandomInterpreter(random)
  }

  class RandomInterpreter(val random: util.Random) extends Random.Handler[mgo.tagtools.Evaluated] {
    def nextDouble() = mgo.tagtools.guard(random.nextDouble())
    def nextInt(n: Int) = mgo.tagtools.guard(random.nextInt(n))
    def nextBoolean() = mgo.tagtools.guard(random.nextBoolean())
    def shuffle[A](s: Vector[A]) = mgo.tagtools.guard(random.shuffle(s))
    def use[T](f: util.Random => T) = mgo.tagtools.guard(f(random))
  }

  def multinomial[M[_]: Monad, T](elements: Vector[(T, Double)])(implicit randomM: Random[M]) = {
    def roulette(weights: List[(T, Double)], selected: Double): M[T] =
      weights match {
        case Nil => randomElement[M, (T, Double)](elements).map(_._1)
        case (i, p) :: t =>
          if (selected <= p) i.pure[M]
          else roulette(t, selected - p)
      }
    randomM.nextDouble.flatMap(d => roulette(elements.toList, d))
  }

  def randomElement[M[_]: Functor, T](v: Vector[T])(implicit randomM: Random[M]) =
    randomM.nextInt(v.size).map(v.apply)

  implicit class RandomDecorator[M[_]](randomM: Random[M]) {
    private implicit def implicitRandomM = randomM
    def multinomial[T](v: Vector[(T, Double)])(implicit monad: Monad[M]) = contexts.multinomial[M, T](v)
    def randomElement[T](v: Vector[T])(implicit functor: Functor[M]) = contexts.randomElement[M, T](v)
  }

  @tagless trait IO {
    def apply[A](f: () => A): FS[A]
  }

  case class IOInterpreter() extends IO.Handler[mgo.tagtools.Evaluated] {
    def apply[A](f: () => A) = mgo.tagtools.guard(f())
  }

  @tagless trait System {
    def randomUUID(): FS[UUID]
    def sleep(duration: Time): FS[Unit]
    def currentTime(): FS[Long]
  }

  case class SystemInterpreter() extends System.Handler[mgo.tagtools.Evaluated] {
    def randomUUID() = mgo.tagtools.guard(UUID.randomUUID())
    def sleep(d: Time) = mgo.tagtools.guard(Thread.sleep(d.millis))
    def currentTime() = mgo.tagtools.guard(java.lang.System.currentTimeMillis())
  }

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
