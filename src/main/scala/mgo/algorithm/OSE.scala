package mgo.algorithm

import mgo.contexts._
import mgo.elitism._
import mgo.niche._

object OSE {

}

object OSEOperation {

  def elitism[M[_]: cats.Monad: Random: Generation: ReachMap, I, N](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    niche: Niche[I, N],
    muByNiche: Int)(implicit archive: Archive[M, I]) = {
    def nsga2Elitism(population: Vector[I]) = NSGA2Operations.elitism[M, I](fitness, values, muByNiche).apply(population)
    Elitism[M, I] { nicheElitism(_, nsga2Elitism, niche) }
  }
}
