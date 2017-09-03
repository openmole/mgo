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
    Elitism(individuals => individuals.sorted(FO.contramap[I](f).reverse.toOrdering).take(mu).pure[M])

  /** Returns n individuals randomly. */
  def randomO[M[_]: cats.Monad, I](n: Int)(implicit randomM: Random[M]): Elitism[M, I] =
    Elitism(
      individuals => Vector.fill(n)(randomM.randomElement(individuals)).sequence
    )

  def incrementGeneration[M[_]: cats.Monad, I](age: monocle.Lens[I, Long])(implicit generationM: Generation[M]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) =>
      for { _ <- generationM.increment } yield individuals.map(age.modify(_ + 1))
    )

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
      val sortedBestToWorst = (population zip ranks).sortBy { _._2 }(KO.reverse.toOrdering).map { _._1 }
      sortedBestToWorst.take(mu)
    }
  }

  type CloneStrategy[M[_], I] = Vector[I] => M[Vector[I]]

  /**** Clone strategies ****/
  def applyCloneStrategy[M[_]: cats.Monad, I, G](getGenome: I => G, cloneStrategy: CloneStrategy[M, I]): Elitism[M, I] =
    Elitism(
      _.groupBy(getGenome).valuesIterator.toVector.flatTraverse(cloneStrategy)
    )

  def keepYoungest[M[_]: cats.Monad, I](age: I => Long): CloneStrategy[M, I] =
    (clones: Vector[I]) => Vector(clones.minBy(age)).pure[M]

  def mergeHistories[M[_]: cats.Monad, I, P](age: monocle.Lens[I, Long], history: monocle.Lens[I, Vector[P]])(historySize: Int): CloneStrategy[M, I] =
    (clones: Vector[I]) =>
      Vector(clones.reduce { (i1, i2) =>
        val (old, young) = if (age.get(i1) > age.get(i2)) (i1, i2) else (i2, i1)

        val oldAge = age.get(old)
        val youngAge = age.get(young)

        def oldH: Vector[P] = history.get(old)
        def youngH: Vector[P] = history.get(young).takeRight(scala.math.min(youngAge, historySize).toInt)
        def updatedHistory = history.set((oldH ++ youngH).takeRight(historySize))(old)
        age.set(oldAge + youngAge)(updatedHistory)
      }).pure[M]

}
