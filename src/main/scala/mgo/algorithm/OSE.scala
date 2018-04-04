package mgo.algorithm

import mgo.contexts._
import mgo.elitism._
import mgo.niche._

object OSE {

}

object OSEOperation {

  def patternIsReached(fitness: Vector[Double], limit: Vector[Double]) =
    (fitness zip limit) forall { case (f, l) => f <= l }

  def elitism[M[_]: cats.Monad: Random: Generation, I, N](
    fitness: I => Vector[Double],
    limit: Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    origin: (Vector[Double], Vector[Int]) => Vector[Int],
    mu: Int)(implicit archive: Archive[M, I], reachMap: ReachMap[M]) = Elitism[M, I] { population =>

    import cats.implicits._

    def o(i: I) = Function.tupled(origin)(values(i))

    def newlyReaching = {
      def keepNewlyReaching(i: I): M[Option[I]] =
        if (patternIsReached(fitness(i), limit))
          (reachMap.reached(o(i))) map {
            case true => None
            case false => Some(i)
          }
        else (None: Option[I]).pure[M]
      population.flatTraverse(i => keepNewlyReaching(i).map(_.toVector))
    }

    def filterReached = {
      def keepNonReaching(i: I): M[Option[I]] =
        reachMap.reached(o(i)) map {
          case true => None
          case false => Some(i)
        }
      population.flatTraverse(i => keepNonReaching(i).map(_.toVector))
    }

    for {
      reaching <- newlyReaching
      _ <- reachMap.setReached(reaching.map(o))
      _ <- archive.put(reaching)
      filteredPopulation <- filterReached
      newPopulation <- NSGA2Operations.elitism[M, I](fitness, values, mu).apply(filteredPopulation)
    } yield newPopulation
  }

}
