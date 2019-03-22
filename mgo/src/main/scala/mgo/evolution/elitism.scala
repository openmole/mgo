package mgo.evolution

import cats.data._
import cats.implicits._
import cats.{ Order, Applicative }
import contexts._
import mgo.tools._
import mgo.tagtools._

object elitism {

  type Elitism[M[_], I] = Kleisli[M, (Vector[I], Vector[I]), Vector[I]]

  object Elitism {
    def apply[M[_]: cats.Monad, I](f: (Vector[I], Vector[I]) => M[Vector[I]]): Elitism[M, I] = Kleisli[M, (Vector[I], Vector[I]), Vector[I]](Function.tupled(f))
  }

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

  def randomO[M[_]: cats.Applicative, I](n: Int)(implicit randomM: Random[M]) =
    (individuals: Vector[I]) => Vector.fill(n)(randomM.randomElement(individuals)).sequence

  def incrementGeneration[M[_]: Generation] = Generation[M].increment

  def addHits[M[_]: cats.Monad: HitMap, I](cell: I => Vector[Int], mapped: monocle.Lens[I, Boolean]) = {
    def hits(cells: Vector[Vector[Int]]) =
      modifier(implicitly[HitMap[M]].get, implicitly[HitMap[M]].set).modify { map =>
        def newValues = cells.map { c => (c, map.getOrElse(c, 0) + 1) }
        map ++ newValues
      }

    Kleisli[M, Vector[I], Vector[I]] { (is: Vector[I]) =>
      for {
        _ <- hits(is.filter(i => !mapped.get(i)).map(cell))
      } yield is.map(mapped.set(true))
    }
  }

  /** Returns the mu individuals with the highest ranks. */
  // FIXME: unbiais when several individuals have the exact same rank (random draw)
  def keepHighestRanked[I, K](population: Vector[I], ranks: Vector[K], mu: Int)(implicit KO: Order[K]) = {
    if (population.size < mu) population
    else {
      val sortedBestToWorst = (population zip ranks).sortBy { _._2 }(Order.reverse(KO).toOrdering).map { _._1 }
      sortedBestToWorst.take(mu)
    }
  }

  def nicheElitism[M[_]: cats.Monad, I, N](population: Vector[I], keep: Vector[I] => M[Vector[I]], niche: I => N): M[Vector[I]] = {
    val niches = population.groupBy(niche).toVector
    niches.map { case (_, individuals) => keep(individuals) }.sequence.map(_.flatten)
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
