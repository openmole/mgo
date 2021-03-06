package mgo.evolution

import cats.Order
import cats.implicits._
import mgo.evolution.algorithm.HitMap
import mgo.evolution.diversity.crowdingDistance
import mgo.tools._

object elitism {

  type Elitism[S, I] = (S, Vector[I], Vector[I], scala.util.Random) => (S, Vector[I])

  //  object Elitism {
  //    def apply[M[_]: cats.Monad, I](f: (Vector[I], Vector[I]) => M[Vector[I]]): Elitism[M, I] = Kleisli[M, (Vector[I], Vector[I]), Vector[I]](Function.tupled(f))
  //  }

  //  def minimiseO[M[_]: Applicative, I, F](f: I => F, mu: Int)(implicit MM: cats.Monad[M], FO: Order[F]): Elitism[M, I] =
  //    Elitism[M, I](individuals => individuals.sorted(FO.contramap[I](f).toOrdering).take(mu).pure[M])
  //
  //  def maximiseO[M[_]: Applicative, I, F](f: I => F, mu: Int)(implicit MM: cats.Monad[M], FO: Order[F]): Elitism[M, I] =
  //    Elitism(individuals => individuals.sorted(Order.reverse(FO.contramap[I](f)).toOrdering).take(mu).pure[M])
  //
  //  /** Returns n individuals randomly. */
  //  def randomO[M[_]: cats.Monad, I](n: Int)(implicit randomM: Random[M]): Elitism[M, I] =
  //    Elitism(
  //      individuals => Vector.fill(n)(randomM.randomElement(individuals)).sequence)

  def maximiseO[I, F](f: I => F, mu: Int)(implicit FO: Order[F]) = (individuals: Vector[I]) =>
    individuals.sorted(Order.reverse(FO.contramap[I](f)).toOrdering).take(mu)

  //  def randomO[M[_]: cats.Applicative, I](n: Int)(implicit randomM: Random[M]) =
  //    (individuals: Vector[I]) => Vector.fill(n)(randomM.randomElement(individuals)).sequence

  //  def incrementGeneration[M[_]: Generation] = Generation[M].increment

  def addHits[I](cell: I => Vector[Int], population: Vector[I], hitmap: HitMap) = {
    def hits(map: HitMap, c: Vector[Int]) = map.updated(c, map.getOrElse(c, 0) + 1)
    population.foldLeft(hitmap)((m, i) => hits(m, cell(i)))
  }

  /** Returns the mu individuals with the highest ranks. */
  def keepHighestRanked[I, K](population: Vector[I], ranks: Vector[K], mu: Int, rng: scala.util.Random)(implicit KO: Order[K]) =
    if (population.size < mu) population
    else {
      val sortedBestToWorst = (population zip ranks).sortBy { _._2 }(Order.reverse(KO).toOrdering).map { _._1 }
      sortedBestToWorst.take(mu)
    }

  def nicheElitism[I, N](population: Vector[I], keep: Vector[I] => Vector[I], niche: I => N) = {
    val niches = population.groupBy(niche).toVector
    niches.flatMap { case (_, individuals) => keep(individuals) }
  }

  def keepFirstFront[I](population: Vector[I], fitness: I => Vector[Double]) =
    if (population.isEmpty) population
    else {
      val dominating = ranking.numberOfDominating(fitness, population)
      val minDominating = dominating.map(_.value).min
      (population zip dominating).filter { case (_, d) => d.value == minDominating }.map(_._1)
    }

  def keepOnFirstFront[I](population: Vector[I], fitness: I => Vector[Double], mu: Int, random: scala.util.Random) = {
    val first = keepFirstFront(population, fitness)
    val crowding = crowdingDistance[I](first, fitness, random)
    keepHighestRanked(first, crowding, mu, random)
  }

  //type UncloneStrategy[M[_], I] = Vector[I] => M[I]

/**** Clone strategies ****/

  //  def applyCloneStrategy[M[_]: cats.Monad, I, G](getGenome: I => G, cloneStrategy: UncloneStrategy[M, I]): Elitism[M, I] = {
  //    import mgo.tools._
  //    def unclone(clones: Vector[I]) =
  //      if (clones.size == 1) clones.head.pure[M]
  //      else cloneStrategy(clones)
  //
  //    Elitism(_.groupByOrdered(getGenome).valuesIterator.map(_.toVector).toVector.traverse(unclone))
  //  }
  //
  //  def keepOldest[M[_]: cats.Monad, I](age: I => Long): UncloneStrategy[M, I] =
  //    (clones: Vector[I]) => clones.maxBy(age).pure[M]
  //
  //  def keepFirst[M[_]: cats.Monad, I]: UncloneStrategy[M, I] =
  //    (clones: Vector[I]) => clones.head.pure[M]

  def keepNiches[I, N](niche: I => N, keep: Vector[I] => Vector[I]) =
    (individuals: Vector[I]) => {
      val indivsByNiche = individuals.groupByOrdered(niche)
      indivsByNiche.values.toVector.map(_.toVector).flatMap(keep.apply)
    }

  def keepFirst[G, I](genome: I => G)(population: Vector[I], newIndividuals: Vector[I]) = {
    val filteredClone = {
      val existingGenomes = population.map(genome).toSet
      newIndividuals.filter(i => !existingGenomes.contains(genome(i)))
    }

    population ++ filteredClone
  }

  def mergeHistories[G, I, P](genome: I => G, history: monocle.Lens[I, Vector[P]], historyAge: monocle.Lens[I, Long], historySize: Int) =
    (population: Vector[I], newIndividuals: Vector[I]) => {
      val mergedClones = {
        val indexedNI = newIndividuals.groupByOrdered(genome)
        for {
          i <- population
          clones = indexedNI.getOrElse(genome(i), List())
        } yield {
          val additionalHistory = clones.flatMap(history.get)
          history.modify(h => (h ++ additionalHistory).takeRight(historySize)) andThen
            historyAge.modify(_ + additionalHistory.size) apply (i)
        }
      }

      val filteredClone = {
        val filter = population.map(genome).toSet
        newIndividuals.filter(i => !filter.contains(genome(i)))
      }

      mergedClones ++ filteredClone
    }

}
