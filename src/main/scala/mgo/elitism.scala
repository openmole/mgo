package mgo

import cats.data._
import cats.implicits._
import freedsl.tool._
import cats.{ Order, Applicative }
import contexts._

object elitism {

  type Elitism[M[_], I] = Kleisli[M, Vector[I], Vector[I]]

  object Elitism {
    def apply[M[_]: cats.Monad, I](f: Vector[I] => M[Vector[I]]): Elitism[M, I] = Kleisli(f)
  }

  def minimiseO[M[_]: Applicative, I, F](f: I => F, mu: Int)(implicit MM: cats.Monad[M], FO: Order[F]): Elitism[M, I] =
    Elitism[M, I](individuals => individuals.sorted(FO.contramap[I](f).toOrdering).take(mu).pure[M])

  def maximiseO[M[_]: Applicative, I, F](f: I => F, mu: Int)(implicit MM: cats.Monad[M], FO: Order[F]): Elitism[M, I] =
    Elitism(individuals => individuals.sorted(Order.reverse(FO.contramap[I](f)).toOrdering).take(mu).pure[M])

  /** Returns n individuals randomly. */
  def randomO[M[_]: cats.Monad, I](n: Int)(implicit randomM: Random[M]): Elitism[M, I] =
    Elitism(
      individuals => Vector.fill(n)(randomM.randomElement(individuals)).sequence)

  def incrementAge[M[_]: cats.Monad, I](age: monocle.Lens[I, Long]) =
    Elitism { (individuals: Vector[I]) => individuals.map(age.modify(_ + 1)).pure[M] }

  def incrementGeneration[M[_]: Generation] = Generation[M].increment

  def addHits[M[_]: cats.Monad, I, C](cell: I => C, mapped: monocle.Lens[I, Boolean])(implicit hitMapperM: HitMap[M, C]) = {
    def hits(cells: Vector[C]) =
      modifier(hitMapperM.get, hitMapperM.set).modify { map =>
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

  type UncloneStrategy[M[_], I] = Vector[I] => M[I]

/**** Clone strategies ****/

  def applyCloneStrategy[M[_]: cats.Monad, I, G](getGenome: I => G, cloneStrategy: UncloneStrategy[M, I]): Elitism[M, I] = {
    import tools._
    def unclone(clones: Vector[I]) =
      if (clones.size == 1) clones.head.pure[M]
      else cloneStrategy(clones)

    Elitism(_.groupByOrdered(getGenome).valuesIterator.map(_.toVector).toVector.traverse(unclone))
  }

  def keepOldest[M[_]: cats.Monad, I](age: I => Long): UncloneStrategy[M, I] =
    (clones: Vector[I]) => clones.maxBy(age).pure[M]

  def keepFirst[M[_]: cats.Monad, I]: UncloneStrategy[M, I] =
    (clones: Vector[I]) => clones.head.pure[M]

  def mergeHistories[M[_]: cats.Monad: Random, I, P](historyAge: monocle.Lens[I, Long], history: monocle.Lens[I, Vector[P]])(historySize: Int): UncloneStrategy[M, I] =
    (clones: Vector[I]) =>
      implicitly[Random[M]].use { rng =>
        def merged: I =
          clones.reduce { (i1, i2) =>
            val i1HistoryAge = historyAge.get(i1)
            val i2HistoryAge = historyAge.get(i2)

            (i1HistoryAge, i2HistoryAge) match {
              case (_, 0) => i1
              case (0, _) => i2
              case _ =>
                def ownHistory(i: I) = history.get(i).takeRight(scala.math.min(historyAge.get(i), historySize).toInt)
                def updatedHistory = history.set(rng.shuffle((ownHistory(i1) ++ ownHistory(i2)).take(historySize)))(i1)
                historyAge.set(i1HistoryAge + i2HistoryAge)(updatedHistory)
            }
          }
        merged
      }

}
