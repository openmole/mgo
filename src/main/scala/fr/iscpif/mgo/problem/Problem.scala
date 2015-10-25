///*
// * Copyright (C) 2012 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.problem
//
//import fr.iscpif.mgo._
//import util.Random
//
///**
// * Definition of a problem
// */
//trait Problem extends Evolution {
//
//  def express(g: G, rng: Random): P
//
//  /**
//   * Evaluate a phenotype
//   *
//   * @param phenotype the phenotype to evaluate
//   * @return the phenotype
//   */
//  def evaluate(phenotype: P, rng: Random): F
//
//  def evolve(implicit rng: Random): Iterator[EvolutionState] = evolve(express _, evaluate _)
//
//  def evolve(genomes: Seq[G])(implicit rng: Random): Iterator[EvolutionState] = evolve(genomes, express _, evaluate _)
//
//  def evolve(population: Population[G, P, F])(implicit rng: Random): Iterator[EvolutionState] = evolve(population, express _, evaluate _)
//
//  def evolve(population: Population[G, P, F], archive: A)(implicit rng: Random): Iterator[EvolutionState] = evolve(population, archive, express _, evaluate _)
//
//  def evolve(population: Population[G, P, F], offspring: Population[G, P, F])(implicit rng: Random): Iterator[EvolutionState] = evolve(population, offspring, express _, evaluate _)
//
//}
