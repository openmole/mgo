/*
 * Copyright (C) 21/05/2015 Guillaume Chérel
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

/**
 * - Init Pop minimally
 * - Evaluate
 * - Loop
 * - Selection
 * - Crossover
 * - Mutation
 * - Evaluation
 * - Elitism
 *
 * # Genome
 *
 * List of connection gene
 *
 * # Connection gene
 *
 * - in-node
 * - out-node
 * - weight
 * - enable bit
 * - innovation number
 *
 * # Selection
 *
 * - every species is assigned a number of offsprings = (species fitness / sum of species fitnesses) * population size
 * - parents are picked at random in each species (only the fittest individuals of each species were selected at the elitism stage)
 *
 * # Crossover
 *
 * - align the 2 parent genomes according to their innovation number
 * - genes that match no other gene in the other genome are disjoint if they occur within the range of the the other genome innovation numbers
 * - they are excess if they occur outside this range.
 * - a new offspring is composed by chosing each matching genes randomly from either parent.
 * - excess or disjoint genes from the fittest parent are included.
 *
 * # Mutation
 *
 * - weight mutation (gaussian)
 * - structural mutation
 * -- add connection (weight = 0?)
 * -- add node: split existing connection
 * --- existing connection is disabled
 * --- create 2 new connection
 * ---- connection leading into the new node has weight 1
 * ---- connection leading out of the new onde has weight equal to the weight of the old connection.
 *
 * Adding a mutation increments the global innovation number.
 *
 * Check for identical mutations in current generation (problem en cas de steady state, pop size = 1).
 *
 * # Evaluation
 *
 * (species attribution can happen at the breeding stage)
 * - the index of species represents each species by a random genome of the corresponding species of the past generation.
 * - each genome is attributed to a species in the index of species if its delta is < delta_t
 * - delta = c1 * E / N + c2 * D / N + c3*avg(W)
 * - when a genome does not belong to any species, a new species is created with it as its representative
 *
 * (When updating the archive)
 * - the index of species is updated by selecting a random genome in each species
 *
 * - the fitness of each genome is computed (the corresponding network must be created and evaluated)
 * - each individual is reattributed a fitness equal to the average fitness of the species.
 *
 * In the orginal NEAT, the fitness sharing function is designed so that higher fitness is better. In MGO, fitness is minimized. Must adapt the fitness sharing function.
 *
 * # Elitism
 *
 * - keep only the 20% fittest individuals of each species.
 *
 * - if the fitness of the entire population does not improve for more than 20 generations, only the two best species are allowed to reproduce.
 *
 * # Speciation
 *
 * - there is an index of species, represented by a random genome of the past generation.
 *
 * Dynamic thresholding
 *
 *
 *
 * # Networks
 *
 * - create a network that can be queried from a list of connection genes (genome)
 *
 *
 * ## querying
 *
 * How do I ensure that a network stabilizes before taking its output(s) for a classification problem?
 *
 * The proper (and quite nice) way to do it is to check every hidden node and output node from one
 * timestep to the next, and see if nothing has changed, or at least not changed within some delta.
 * Once this criterion is met, the output must be stable.
 *
 * Note that output may not always stabilize in some cases. Also, for continuous control problems,
 * do not check for stabilization as the network never "settles" but rather continuously reacts to a
 * changing environment. Generally, stabilization is used in classification problems, or in board games. "
 *
 * # Testing
 *
 * see "How should I test my own version of NEAT to make sure it works?"
 * http://www.cs.ucf.edu/~kstanley/neat.html
 *
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import fr.iscpif.mgo.problem.NEATProblem

/**
 * Differences with the original neat:
 * - can start with an unconnected genome
 * - can choose between using species hint or not
 * - can mutate weights to 0 to enforce sparsity
 * - On ne normalise pas la distance entre génomes par le génome le plus grand, et on prend la somme des différences des poids plutôt que la moyenne
 */
trait NEAT <: NEATProblem with NEATBreeding with NEATElitism with NEATArchive with NoPhenotype
