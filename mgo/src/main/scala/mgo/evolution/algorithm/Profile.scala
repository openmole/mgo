/*
 * Copyright (C) 15/12/2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mgo.evolution.algorithm

import mgo.evolution._
import mgo.evolution.algorithm.GenomeVectorDouble._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.evolution.niche._
import mgo.tools.*

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

object Profile {

  import CDGenome._
  import DeterministicIndividual._

  type ProfileState = EvolutionState[Unit]

  case class Result[N, P](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N, individual: Individual[P])

  def result[N, P](population: Vector[Individual[P]], niche: Individual[P] => N, continuous: Vector[C], discrete: Vector[D], fitness: P => Vector[Double], keepAll: Boolean): Vector[Result[N, P]] =
    val individuals = if (keepAll) population else nicheElitism[Individual[P], N](population, keepFirstFront(_, individualFitness(fitness)), niche)

    individuals.map: i =>
      Result(
        scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous),
        i.focus(_.genome) andThen discreteVectorValues(discrete) get,
        individualFitness(fitness)(i),
        niche(i),
        i)

  def continuousProfile[P](continuous: Vector[C], x: Int, nX: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.continuousProfile[Individual[P]](_.focus(_.genome) andThen continuousVectorValues(continuous) get, x, nX)

  def discreteProfile[P](discrete: Vector[D], x: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.discreteProfile[Individual[P]](_.focus(_.genome) andThen discreteVectorValues(discrete) get, x)

  def boundedContinuousProfile[P](continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](i => scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile[P](continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](i => scaleContinuousVectorValues(continuousVectorValues(continuous).get(i.genome), continuous), x, intervals)

  //  def boundedObjectiveProfile(x: Int, nX: Int, min: Double, max: Double): Niche[Individual, Int] =
  //    mgo.evolution.niche.boundedContinuousProfile[Individual](vectorPhenotype.get _, x, nX, min, max)
  //
  //  def gridObjectiveProfile(x: Int, intervals: Vector[Double]): Niche[Individual, Int] =
  //    mgo.evolution.niche.gridContinuousProfile[Individual](vectorPhenotype.get _, x, intervals)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], reject: Option[Genome => Boolean], rng: scala.util.Random): Vector[Genome] =
    CDGenome.initialGenomes(lambda, continuous, discrete, reject, rng)

  def adaptiveBreeding[P](lambda: Int, operatorExploration: Double, continuous: Vector[C], discrete: Vector[D], fitness: P => Vector[Double], reject: Option[Genome => Boolean]): Breeding[ProfileState, Individual[P], Genome] =
    NSGA2Operations.adaptiveBreeding[ProfileState, Individual[P], Genome](
      individualFitness(fitness),
      Focus[Individual[P]](_.genome).get,
      continuousValues(continuous).get,
      continuousOperator.get,
      discreteValues(discrete).get,
      discreteOperator.get,
      continuous,
      discrete,
      buildGenome(discrete),
      logOfPopulationSize,
      lambda,
      reject,
      operatorExploration)

  def expression[P](express: (IArray[Double], IArray[Int]) => P, components: Vector[C], discrete: Vector[D]) =
    DeterministicIndividual.expression(express, components, discrete)

  def elitism[N, P](niche: Niche[Individual[P], N], mu: Int, components: Vector[C], discrete: Vector[D], fitness: P => Vector[Double]): Elitism[ProfileState, Individual[P]] =
    ProfileOperations.elitism[ProfileState, Individual[P], N](
      individualFitness(fitness),
      i => scaledValues(components, discrete)(i.genome),
      niche,
      mu)

  given isAlgorithm[N]: Algorithm[Profile[N], Individual[Vector[Double]], Genome, ProfileState] with {
    override def initialState(t: Profile[N], rng: scala.util.Random) = EvolutionState(s = ())

    def initialPopulation(t: Profile[N], rng: scala.util.Random, parallel: Algorithm.ParallelContext) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        Profile.initialGenomes(t.lambda, t.continuous, t.discrete, reject(t), rng),
        Profile.expression(t.fitness, t.continuous, t.discrete),
        parallel)

    def step(t: Profile[N]) =
      deterministic.step[ProfileState, Individual[Vector[Double]], Genome](
        Profile.adaptiveBreeding(t.lambda, t.operatorExploration, t.continuous, t.discrete, identity, reject(t)),
        Profile.expression(t.fitness, t.continuous, t.discrete),
        Profile.elitism(t.niche, t.nicheSize, t.continuous, t.discrete, identity),
        Focus[ProfileState](_.generation),
        Focus[ProfileState](_.evaluated))

  }

  def reject[N](profile: Profile[N]): Option[Genome => Boolean] = NSGA2.reject(profile.reject, profile.continuous, profile.discrete)

  def result[N](profile: Profile[N], population: Vector[Individual[Vector[Double]]]): Vector[Result[N, Vector[Double]]] =
    result[N, Vector[Double]](population, profile.niche, profile.continuous, profile.discrete, identity, keepAll = false)

}

case class Profile[N](
  lambda: Int,
  fitness: (IArray[Double], IArray[Int]) => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  niche: Niche[CDGenome.DeterministicIndividual.Individual[Vector[Double]], N],
  nicheSize: Int = 20,
  operatorExploration: Double = 0.1,
  reject: Option[(IArray[Double], IArray[Int]) => Boolean] = None)

object ProfileOperations:

  def elitism[S, I, N](
    fitness: I => Vector[Double],
    values: I => (IArray[Double], IArray[Int]),
    niche: Niche[I, N],
    muByNiche: Int): Elitism[S, I] =
    (s, population, candidates, rng) =>
      val memoizedFitness = fitness.memoized
      val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), memoizedFitness)
      def nsga2Elitism(p: Vector[I]) = NSGA2Operations.elitism[S, I](memoizedFitness, values, muByNiche).apply(s, p, Vector.empty, rng)._2
      val newPopulation = nicheElitism(cloneRemoved, nsga2Elitism, niche)
      (s, newPopulation)

