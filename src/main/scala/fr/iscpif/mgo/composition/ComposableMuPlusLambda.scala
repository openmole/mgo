/*
 * Copyright (C) 20/11/12 Romain Reuillon
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

package fr.iscpif.mgo.composition

import fr.iscpif.mgo._
import java.util.Random

class ComposableMuPlusLambda[CG <: Genome, CF <: Fitness, CMF, CA, CSTATE](
    val archiveComponent: Archive { type A = CA; type G = CG; type F = CF; type MF = CMF },
    val modifierComponent: Modifier { type G = CG; type F = CF; type MF = CMF; type A = CA },
    val selectionComponent: Selection { type G = CG; type F = CF; type MF = CMF },
    val genomeFactoryComponent: Factory[CG],
    val crossOverComponent: CrossOver { type G = CG; type F = CF; type MF = CMF },
    val mutationComponent: Mutation { type G = CG; type F = CF; type MF = CMF },
    val elitismComponent: Elitism { type G = CG; type F = CF; type MF = CMF },
    val terminationComponent: Termination { type G = CG; type F = CF; type MF = CMF; type STATE = CSTATE },
    val lambda: Int) extends Evolution with Elitism with Breeding with MuPlusLambda {

  type A = CA
  type G = CG
  type F = CF
  type MF = CMF
  type STATE = CSTATE

  def initialArchive = archiveComponent.initialArchive
  def archive(archive: A, individuals: Seq[Individual[G, F]]) = archiveComponent.archive(archive, individuals)
  def modify(individuals: Seq[Individual[G, F]], archive: A) = modifierComponent.modify(individuals, archive)
  def selection(population: Population[G, F, MF])(implicit aprng: Random) = selectionComponent.selection(population)
  def crossover(g1: G, g2: G)(implicit aprng: Random) = crossOverComponent.crossover(g1, g2)
  def mutate(genome: G)(implicit aprng: Random) = mutationComponent.mutate(genome)
  def elitism(population: Population[G, F, MF]) = elitismComponent.elitism(population)
  def initialState(p: Population[G, F, MF]) = terminationComponent.initialState(p)
  def terminated(population: Population[G, F, MF], terminationState: STATE) = terminationComponent.terminated(population, terminationState)
  def genomeFactory = genomeFactoryComponent
}
