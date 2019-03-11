package mgo

import cats._
import cats.implicits._
import cats.data.Kleisli

package object tagtools {
  import scala.language.experimental.macros
  import scala.reflect.macros._

  //  def repeatImpl[M[_], A](c: whitebox.Context)(m: c.Expr[M[A]], size: c.Expr[Int]): c.Expr[M[Vector[A]]] = {
  //    import c.universe._
  //    c.Expr[M[Vector[A]]](q"_root_.freedsl.tool.`macro`.repeatCall(() => ${m}, ${size})")
  //  }
  //
  //  def repeat[M[_], A](m: M[A], size: Int): M[Vector[A]] = macro repeatImpl[M, A]
  //
  //  def untilImpl[M[_], A](c: whitebox.Context)(m: c.Expr[M[A]], end: c.Expr[Kleisli[M, A, Boolean]]): c.Expr[M[A]] = {
  //    import c.universe._
  //    c.Expr[M[A]](q"_root_.freedsl.tool.`macro`.untilCall(() => ${m}, ${end})")
  //  }
  //
  //  def until[M[_], A](m: M[A], end: Kleisli[M, A, Boolean]): M[A] = macro untilImpl[M, A]

  def convertImpl[M[_], A](c: whitebox.Context)(m: c.Expr[M[A]]): c.Expr[() => M[A]] = {
    import c.universe._
    c.Expr[() => M[A]](q"() => $m")
  }

  implicit def convert[M[_], A](m: M[A]) = macro convertImpl[M, A]

  def repeat[M[_]: Monad, A](m: () => M[A], size: Int) = {
    type Rec = (List[A], Int)

    def stop(a: List[A]): Either[Rec, List[A]] = Right(a)
    def continue(a: List[A], size: Int): Either[Rec, List[A]] = Left((a, size))

    def loop = Monad[M].tailRecM[Rec, List[A]]((List.empty, 0)) {
      case (list, s) =>
        if (s < size) m().map { a => continue(a :: list, s + 1) }
        else stop(list).pure[M]
    }

    loop.map { _.reverse.toVector }
  }

  def until[M[_]: Monad, A](m: () => M[A], end: Kleisli[M, A, Boolean]): M[A] = {
    def stop(a: A): Either[Unit, A] = Right(a)
    val continue: Either[Unit, A] = Left(Unit)

    def loop = Monad[M].tailRecM[Unit, A](Unit) { i =>
      val comp =
        for {
          a <- m()
          b <- end.run(a)
        } yield (b, a)

      comp.map { case (e, a) => (if (e) stop(a) else continue) }
    }

    loop
  }

  //    (0 until size).toVector.traverse(i => m())
  // def until[M[_], A](m: M[A], end: A => Boolean): M[A] = until(m, Kleisli[M, A, Boolean](a => end(a).pure[M]))

  //def until(end: Kleisli[M, A, Boolean]) = Kleisli { a: A => fold(a)(end) }

  implicit class MonadDecorator[M[_]: Monad, A](m: M[A]) {
    def until(end: M[Boolean]): M[A] = until(Kleisli[M, A, Boolean](_ => end))
    def until(end: A => Boolean): M[A] = until(Kleisli[M, A, Boolean](a => end(a).pure[M]))

    def until(end: Kleisli[M, A, Boolean]): M[A] = {
      def stop(a: A): Either[Unit, A] = Right(a)
      val continue: Either[Unit, A] = Left(Unit)

      def loop = Monad[M].tailRecM[Unit, A](Unit) { i =>
        val comp =
          for {
            a <- m
            b <- end.run(a)
          } yield (b, a)

        comp.map { case (e, a) => (if (e) stop(a) else continue) }
      }

      loop
    }

    def repeat(size: Int): M[Vector[A]] = mgo.tagtools.repeat[M, A](() => m, size)

  }

  implicit class MonadVectorDecorator[M[_]: Monad, A](m: M[Vector[A]]) {

    def accumulate(size: Int): M[Vector[A]] = {
      def stop(a: Vector[A]): Either[Vector[A], Vector[A]] = Right(a)

      def continue(v: Vector[A]): Either[Vector[A], Vector[A]] = Left(v)

      def loop(init: Vector[A]) = Monad[M].tailRecM[Vector[A], Vector[A]](init) {
        case v =>
          val comp =
            for {
              a <- m
              acc = v ++ a
            } yield (acc.size >= size, acc)

          comp.map { case (e, a) => (if (e) stop(a) else continue(a)) }
      }

      for {
        init <- m
        res <- if (init.size >= size) init.pure[M] else loop(init)
      } yield res
    }
  }

  implicit class KleisliDecorator[M[_]: Monad, A](m: Kleisli[M, A, A]) {
    def until(end: Kleisli[M, A, Boolean]) = Kleisli { a: A => fold(a)(end) }

    def fold(a: A)(end: Kleisli[M, A, Boolean]): M[A] = {
      def stop(a: A): Either[A, A] = Right(a)
      def continue(a: A): Either[A, A] = Left(a)

      def loop = Monad[M].tailRecM[A, A](a) { a =>
        val comp =
          for {
            ar <- m.run(a)
            b <- end.run(ar)
          } yield (b, ar)

        comp.map { case (e, a) => (if (e) stop(a) else continue(a)) }
      }

      loop
    }
  }

  def noop[M[_]: Applicative]: M[Unit] = Applicative[M].pure(())

  def modifier[F[_]: Monad, T](get: F[T], set: T => F[Unit]) = new {
    def modify(f: T => T) =
      for {
        v <- get
        nv = f(v)
        _ <- set(nv)
      } yield nv

    def apply(f: T => T) = modify(f)
  }

  implicit class TryDecorator[T](t: util.Try[T]) {
    def mapFailure(f: Throwable ⇒ Throwable) =
      t match {
        case util.Success(s) ⇒ util.Success(s)
        case util.Failure(fa) ⇒ util.Failure(f(fa))
      }
  }

  def error(f: => Throwable) = Left(f)
  def result[T](f: => T) = Right(f)
  def guard[T](f: => T) =
    try result(f)
    catch {
      case t: Throwable => error(t)
    }

  type Evaluated[T] = Either[Throwable, T]
  type DSL[T] = cats.free.Free[Evaluated, T]

  implicit class DSLDecorator[T](f: DSL[T]) {
    def eval = tryEval.get
    def tryEval = f.runTailRec.toTry
  }

  implicit def freestyleTaglessLiftFree[F[_]] = freestyle.tagless.freestyleTaglessLiftFree[F]
}
