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
import mgo.tools.execution._

import scala.language.higherKinds

object Profile {

  import CDGenome._
  import DeterministicIndividual._

  type ProfileState = EvolutionState[Unit]

  case class Result[N](continuous: Vector[Double], discrete: Vector[Int], fitness: Vector[Double], niche: N)

  def result[N, P](population: Vector[Individual[P]], niche: Individual[P] => N, continuous: Vector[C], fitness: P => Vector[Double]) =
    nicheElitism[Individual[P], N](population, keepFirstFront(_, individualFitness(fitness)), niche).map { i =>
      Result(
        scaleContinuousValues(continuousValues.get(i.genome), continuous),
        Individual.genome composeLens discreteValues get i,
        individualFitness(fitness)(i),
        niche(i))
    }

  def continuousProfile[P](x: Int, nX: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.continuousProfile[Individual[P]]((Individual.genome composeLens continuousValues).get _, x, nX)

  def discreteProfile[P](x: Int): Niche[Individual[P], Int] =
    mgo.evolution.niche.discreteProfile[Individual[P]]((Individual.genome composeLens discreteValues).get _, x)

  def boundedContinuousProfile[P](continuous: Vector[C], x: Int, nX: Int, min: Double, max: Double): Niche[Individual[P], Int] =
    mgo.evolution.niche.boundedContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, nX, min, max)

  def gridContinuousProfile[P](continuous: Vector[C], x: Int, intervals: Vector[Double]): Niche[Individual[P], Int] =
    mgo.evolution.niche.gridContinuousProfile[Individual[P]](i => scaleContinuousValues(continuousValues.get(i.genome), continuous), x, intervals)

  //  def boundedObjectiveProfile(x: Int, nX: Int, min: Double, max: Double): Niche[Individual, Int] =
  //    mgo.evolution.niche.boundedContinuousProfile[Individual](vectorPhenotype.get _, x, nX, min, max)
  //
  //  def gridObjectiveProfile(x: Int, intervals: Vector[Double]): Niche[Individual, Int] =
  //    mgo.evolution.niche.gridContinuousProfile[Individual](vectorPhenotype.get _, x, intervals)

  def initialGenomes(lambda: Int, continuous: Vector[C], discrete: Vector[D], rng: scala.util.Random) =
    CDGenome.initialGenomes(lambda, continuous, discrete, rng)

  def adaptiveBreeding[P](lambda: Int, operatorExploration: Double, discrete: Vector[D], fitness: P => Vector[Double]) =
    NSGA2Operations.adaptiveBreeding[ProfileState, Individual[P], Genome](
      individualFitness(fitness),
      Individual.genome.get,
      continuousValues.get,
      continuousOperator.get,
      discreteValues.get,
      discreteOperator.get,
      discrete,
      buildGenome,
      logOfPopulationSize,
      lambda,
      operatorExploration)

  def expression[P](express: (Vector[Double], Vector[Int]) => P, components: Vector[C]): Genome => Individual[P] =
    DeterministicIndividual.expression(express, components)

  def elitism[N, P](niche: Niche[Individual[P], N], mu: Int, components: Vector[C], fitness: P => Vector[Double]) =
    ProfileOperations.elitism[ProfileState, Individual[P], N](
      individualFitness(fitness),
      i => values(Individual.genome.get(i), components),
      niche,
      mu)

  implicit def isAlgorithm[N]: Algorithm[Profile[N], Individual[Vector[Double]], Genome, ProfileState] = new Algorithm[Profile[N], Individual[Vector[Double]], Genome, ProfileState] {
    override def initialState(t: Profile[N], rng: scala.util.Random) = EvolutionState(s = Unit)

    def initialPopulation(t: Profile[N], rng: scala.util.Random) =
      deterministic.initialPopulation[Genome, Individual[Vector[Double]]](
        Profile.initialGenomes(t.lambda, t.continuous, t.discrete, rng),
        Profile.expression(t.fitness, t.continuous))

    def step(t: Profile[N]) =
      deterministic.step[ProfileState, Individual[Vector[Double]], Genome](
        Profile.adaptiveBreeding(t.lambda, t.operatorExploration, t.discrete, identity),
        Profile.expression(t.fitness, t.continuous),
        Profile.elitism(t.niche, t.nicheSize, t.continuous, identity),
        EvolutionState.generation)

  }

  def result[N](profile: Profile[N], population: Vector[Individual[Vector[Double]]]): Vector[Result[N]] =
    result[N, Vector[Double]](population, profile.niche, profile.continuous, identity)

}

case class Profile[N](
  lambda: Int,
  fitness: (Vector[Double], Vector[Int]) => Vector[Double],
  continuous: Vector[C] = Vector.empty,
  discrete: Vector[D] = Vector.empty,
  niche: Niche[CDGenome.DeterministicIndividual.Individual[Vector[Double]], N],
  nicheSize: Int = 20,
  operatorExploration: Double = 0.1)

object ProfileOperations {

  def elitism[S, I, N](
    fitness: I => Vector[Double],
    values: I => (Vector[Double], Vector[Int]),
    niche: Niche[I, N],
    muByNiche: Int): Elitism[S, I] =
    (s, population, candidates, rng) => {
      val cloneRemoved = filterNaN(keepFirst(values)(population, candidates), fitness)
      def nsga2Elitism(p: Vector[I]) = NSGA2Operations.elitism[S, I](fitness, values, muByNiche).apply(s, p, Vector.empty, rng)._2
      val newPopulation = nicheElitism(cloneRemoved, nsga2Elitism, niche)
      (s, newPopulation)
    }
}
