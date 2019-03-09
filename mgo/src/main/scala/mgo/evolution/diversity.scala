package mgo.evolution

import cats.data._
import cats.implicits._
import contexts._

import mgo.tools.metric._
import shapeless._

/**
 * Layer of the cake that compute a diversity metric for a set of values
 */
object diversity {

  /** Compute the diversity metric of the values */
  type Diversity[M[_], I] = Kleisli[M, Vector[I], Vector[Lazy[Double]]]
  object Diversity {
    def apply[M[_]: cats.Monad, I](f: Vector[I] => M[Vector[Lazy[Double]]]): Diversity[M, I] = Kleisli[M, Vector[I], Vector[Lazy[Double]]](f)
  }

  /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance[M[_]: cats.Monad: Random, I](fitness: I => Vector[Double]): Diversity[M, I] =
    Diversity((values: Vector[I]) => CrowdingDistance[M](values.map(e => fitness(e))))

  def hypervolumeContribution[M[_]: cats.Monad, I](referencePoint: Vector[Double], fitness: I => Vector[Double]): Diversity[M, I] =
    Diversity((values: Vector[I]) => Hypervolume.contributions(values.map(e => fitness(e)), referencePoint).pure[M])

  def KNearestNeighbours[M[_], I](k: Int, fitness: I => Vector[Double])(implicit MM: cats.Monad[M]): Diversity[M, I] =
    Diversity((values: Vector[I]) => KNearestNeighboursAverageDistance(values.map(e => fitness(e)), k).pure[M])

}
