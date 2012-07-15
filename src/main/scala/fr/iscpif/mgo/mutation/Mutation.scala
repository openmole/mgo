/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.mutation

//TODO : creer un decorateur qui permet d'automatiquement encapsulé un genome simple dans un indexedSeq pour les besoins de genericité entre
//crossover(retour de 2 genomes) et mutation (retour de 1 genome)'
import fr.iscpif.mgo._
import java.util.Random

trait Mutation { this: Evolution =>
  def mutate(genomes: G) (implicit aprng : Random, factory: Factory[G]): G
}
