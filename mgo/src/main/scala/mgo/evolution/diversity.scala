package mgo.evolution

import cats._
import cats.data._
import cats.implicits._

import mgo.tools.metric._

/**
 * Layer of the cake that compute a diversity metric for a set of values
 */
object diversity:

  /** Compute the diversity metric of the values */
  type Diversity[M[_], I] = Kleisli[M, Vector[I], Vector[Later[Double]]]
  object Diversity:
    def apply[M[_]: cats.Monad, I](f: Vector[I] => M[Vector[Later[Double]]]): Diversity[M, I] = Kleisli[M, Vector[I], Vector[Later[Double]]](f)

  /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance[I](population: Vector[I], fitness: I => Vector[Double], random: scala.util.Random): Vector[Double] = CrowdingDistance(population.map(e => fitness(e)), random)

  def hypervolumeContribution[M[_]: cats.Monad, I](referencePoint: Vector[Double], fitness: I => Vector[Double]): Diversity[M, I] =
    Diversity((values: Vector[I]) => Hypervolume.contributions(values.map(e => fitness(e)), referencePoint).pure[M])

  def KNearestNeighbours[M[_], I](k: Int, fitness: I => Vector[Double])(implicit MM: cats.Monad[M]): Diversity[M, I] =
    Diversity((values: Vector[I]) => KNearestNeighboursAverageDistance(values.map(e => fitness(e)), k).pure[M])


