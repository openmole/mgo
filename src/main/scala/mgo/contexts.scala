package mgo

import cats._
import freedsl.dsl._

object contexts {

  //  object Random {
  //    def interpreter(rng: util.Random) = new Interpreter[Id] {
  //      def interpret[_] = {
  //        case get() => Right(rng)
  //        case shuffle(s) => Right(rng.shuffle(s))
  //      }
  //    }
  //  }
  //
  //  @dsl trait Random[M[_]] {
  //    /** returns the random number generator in M */
  //    def get: M[util.Random]
  //    def shuffle[A](s: Seq[A]): M[Seq[A]]
  //  }

  object Generation {
    def interpreter(g: Long) = new Interpreter {
      var generation = g
      def get(implicit context: Context) = success(generation)
      def increment(implicit context: Context) = success(generation += 1)
    }
  }

  @dsl trait Generation[M[_]] {
    def get: M[Long]
    def increment: M[Unit]
  }

  object StartTime {
    def interpreter(startTime: Long) = new Interpreter {
      def get(implicit context: Context) = success(startTime)
    }
  }

  @dsl trait StartTime[M[_]] {
    def get: M[Long]
  }

  trait HitMap[M[_], C] {
    def get: M[Map[C, Int]]
    def set(map: Map[C, Int]): M[Unit]
  }

  //  @Lenses case class EvolutionState[S](
  //    generation: Long = 0,
  //    startTime: Long = System.currentTimeMillis(),
  //    random: util.Random = newRNG(System.currentTimeMillis()),
  //    s: S)

  //    type DSL = Random.DSL :||: ParallelRandom.DSL :||: Generation.DSL :||: StartTime.DSL
  //    val DSL = freek.DSL.Make[DSL]
  //    type Context[T] = Free[DSL.Cop, T]

  //
  //    type EvolutionState[S, T] = StateT[IO, EvolutionData[S], T]
  //
  //    def unwrap[S, T](x: EvolutionState[S, T], s: EvolutionData[S]): (EvolutionData[S], T) = x.run(s).unsafePerformIO
  //
  //    implicit def evolutionStateMonad[S]: Monad[EvolutionState[S, ?]] = StateT.stateTMonadState[EvolutionData[S], IO]
  //    implicit def evolutionStateMonadState[S]: MonadState[EvolutionState[S, ?], EvolutionData[S]] = StateT.stateTMonadState[EvolutionData[S], IO]
  //    implicit def evolutionStateMonadTrans[S]: MonadTrans[StateT[?[_], EvolutionData[S], ?]] = StateT.StateMonadTrans[EvolutionData[S]]

  /*implicit def lensToField[A, S](l: monocle.Lens[S, A]): Field[EvolutionState[S, ?], A] =
      (f: A => A) =>
        for {
          _ <- evolutionStateMonadState[S].modify((EvolutionData.s composeLens l).modify(f))
          a <- evolutionStateMonadState[S].gets(s => (EvolutionData.s composeLens l).get(s))
        } yield a*/

  //    implicit def evolutionStateUseRG[S]: RandomGen[EvolutionState[S, ?]] = new RandomGen[EvolutionState[S, ?]] {
  //      def random: EvolutionState[S, Random] =
  //        for {
  //          s <- evolutionStateMonadState[S].get
  //        } yield s.random
  //    }
  //
  //    implicit def evolutionStateUseParallelRG[S]: ParallelRandomGen[EvolutionState[S, ?]] = new ParallelRandomGen[EvolutionState[S, ?]] {
  //      def split: EvolutionState[S, Random] =
  //        for {
  //          s <- evolutionStateMonadState[S].get
  //          rg = s.random
  //          rg1 = newRNG(rg.nextLong())
  //        } yield rg1
  //    }
  //
  //    implicit def evolutionStateGenerational[S]: Generational[EvolutionState[S, ?]] = new Generational[EvolutionState[S, ?]] {
  //      def getGeneration: EvolutionState[S, Long] = evolutionStateMonadState[S].get.map(_.generation)
  //
  //      def setGeneration(i: Long): EvolutionState[S, Unit] =
  //        for {
  //          s <- evolutionStateMonadState[S].get
  //          _ <- evolutionStateMonadState[S].put(s.copy(generation = i))
  //        } yield ()
  //
  //      def incrementGeneration: EvolutionState[S, Unit] =
  //        getGeneration >>= { generation => setGeneration(generation + 1) }
  //
  //      def generationReached(x: Long): EvolutionState[S, Boolean] =
  //        for {
  //          g <- getGeneration
  //        } yield g >= x
  //    }
  //
  //    implicit def evolutionStartTime[S]: StartTime[EvolutionState[S, ?]] = new StartTime[EvolutionState[S, ?]] {
  //      def startTime: EvolutionState[S, Long] = evolutionStateMonadState[S].get.map(_.startTime)
  //    }
  //
  //    def liftIOValue[S, A](mio: EvolutionState[S, IO[A]]): EvolutionState[S, A] =
  //      for {
  //        ioa <- mio
  //        a <- evolutionStateMonadTrans[S].liftM[IO, A](ioa)
  //      } yield a
  //
  //    def runEAUntilStackless[S, I](
  //      stopCondition: StopCondition[EvolutionState[S, ?], I],
  //      stepFunction: Kleisli[EvolutionState[S, ?], Vector[I], Vector[I]]): Kleisli[EvolutionState[S, ?], Vector[I], Vector[I]] = {
  //
  //      @tailrec
  //      def tailRec(start: EvolutionData[S], population: Vector[I]): EvolutionState[S, Vector[I]] = {
  //        val (s1, stop) = stopCondition.run(population).run(start).unsafePerformIO
  //        if (stop) StateT.apply[IO, EvolutionData[S], Vector[I]] { _ => IO((s1, population)) }
  //        else {
  //          val (s2, newpop) = stepFunction.run(population).run(s1).unsafePerformIO
  //          tailRec(s2, newpop)
  //        }
  //      }
  //
  //      Kleisli.kleisli[EvolutionState[S, ?], Vector[I], Vector[I]] { population: Vector[I] =>
  //        for {
  //          start <- evolutionStateMonadState[S].get
  //          res <- tailRec(start, population)
  //        } yield res
  //      }
  //    }
  //
}
