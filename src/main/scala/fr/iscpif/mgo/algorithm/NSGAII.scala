/*
 * Copyright (C) 22/05/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._

trait NSGAII <: Evolution
  with GAGenomeWithSigma
  with BinaryTournamentSelection
  with TournamentOnRankAndDiversity
  with CounterTermination
  with NonDominatedElitism
  with CoEvolvingSigmaValuesMutation
  with SBXBoundedCrossover
  with CrowdingDiversity
  with ParetoRanking
  with StrictDominance
  with NoArchive
  with CloneRemoval
  with GeneticBreeding
  with MGFitness
  with ClampedGenome