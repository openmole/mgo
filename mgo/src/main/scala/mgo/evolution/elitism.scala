package mgo.evolution

import cats.Order
import cats.implicits._
import mgo.evolution.algorithm.HitMap
import mgo.evolution.diversity.crowdingDistance
import mgo.tools._

object elitism:

  extension [A](t: Iterable[A])
    def groupByOrdered[K: ImplementEqualMethod as eqm](f: A => K): collection.Map[ImplementEqualMethod.EQM[K], List[A]] =
      val map = collection.mutable.LinkedHashMap[ImplementEqualMethod.EQM[K], List[A]]()
      for (i <- t)
        do
          val key = eqm(f(i))
          map(key) = i :: map.getOrElse(key, Nil)

      map.mapValues(_.reverse).toMap

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

  def maximiseO[I, F](f: I => F, mu: Int)(implicit FO: Order[F]): Vector[I] => Vector[I] = (individuals: Vector[I]) =>
    individuals.sorted(Order.reverse(FO.contramap[I](f)).toOrdering).take(mu)

  //  def randomO[M[_]: cats.Applicative, I](n: Int)(implicit randomM: Random[M]) =
  //    (individuals: Vector[I]) => Vector.fill(n)(randomM.randomElement(individuals)).sequence

  //  def incrementGeneration[M[_]: Generation] = Generation[M].increment

  def addHits[I](cell: I => Vector[Int], population: Vector[I], hitmap: HitMap): HitMap =
    def hits(map: HitMap, c: Vector[Int]) = map.updated(c, map.getOrElse(c, 0) + 1)
    population.foldLeft(hitmap)((m, i) => hits(m, cell(i)))

  /** Returns the mu individuals with the highest ranks. */
  def keepHighestRanked[I, K: Order as KO](population: Vector[I], ranks: Vector[K], mu: Int): Vector[I] =
    if population.size < mu
    then population
    else
      val sortedBestToWorst = (population zip ranks).sortBy { _._2 }(using Order.reverse(KO).toOrdering).map { _._1 }
      sortedBestToWorst.take(mu)

  def nicheElitism[I, N](population: Vector[I], keep: Vector[I] => Vector[I], niche: I => N): Vector[I] =
    val niches = population.groupBy(niche).toVector
    niches.flatMap { case (_, individuals) => keep(individuals) }

  def keepFirstFront[I](population: Vector[I], fitness: I => Vector[Double]): Vector[I] =
    if population.isEmpty
    then population
    else
      val dominating = ranking.numberOfDominating(fitness, population)
      val minDominating = dominating.map(_.value).min
      (population zip dominating).filter((_, d) => d.value == minDominating).map(_._1)

  def keepOnFirstFront[I](population: Vector[I], fitness: I => Vector[Double], mu: Int, random: scala.util.Random): Vector[I] =
    val first = keepFirstFront(population, fitness)
    val crowding = crowdingDistance(first, fitness)
    val res = keepHighestRanked(first, crowding, mu)
    res

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

  def keepRandomElementInNiches[I, N: ImplementEqualMethod as eqm](niche: I => N, random: scala.util.Random): Vector[I] => Vector[I] = individuals =>
    val indivsByNiche = individuals.groupByOrdered(niche)
    indivsByNiche.values.toVector.map(_.toVector).flatMap: individuals =>
      if individuals.isEmpty
      then individuals
      else Vector(individuals(random.nextInt(individuals.size)))

  def keepNiches[I, N: ImplementEqualMethod as eqm](niche: I => N, keep: Vector[I] => Vector[I]): Vector[I] => Vector[I] =
    (individuals: Vector[I]) =>
      val indivsByNiche = individuals.groupByOrdered(niche)
      indivsByNiche.values.toVector.map(_.toVector).flatMap(keep.apply)

  def keepFirst[G: ImplementEqualMethod as eqm, I](genome: I => G)(population: Vector[I], newIndividuals: Vector[I]): Vector[I] =
    val filteredClone =
      val existingGenomes = population.map(genome).map(eqm.apply).toSet
      newIndividuals.filter: i =>
        !existingGenomes.contains(eqm(genome(i)))

    population ++ filteredClone

  def mergeHistories[G: ImplementEqualMethod, I, P](genome: I => G, history: monocle.Lens[I, Vector[P]], historyAge: monocle.Lens[I, Long], historySize: Int): (Vector[I], Vector[I]) => Vector[I] =
    (population: Vector[I], newIndividuals: Vector[I]) =>
      val mergedClones =
        val indexedNI = newIndividuals.groupByOrdered(genome)

        for
          i <- population
          clones = indexedNI.getOrElse(ImplementEqualMethod(genome(i)), List())
        yield
          val additionalHistory = clones.flatMap(history.get)
          history.modify(h => (h ++ additionalHistory).takeRight(historySize)) andThen
            historyAge.modify(_ + additionalHistory.size) apply i

      val filteredClone =
        val filter = population.map(genome andThen ImplementEqualMethod.apply).toSet
        newIndividuals.filter(i => !filter.contains(ImplementEqualMethod(genome(i))))

      mergedClones ++ filteredClone

