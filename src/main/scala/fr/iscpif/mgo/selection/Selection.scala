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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import java.util.Random

// TODO : on ne peut pas etendre de operator, car on traite des individu ici et non pas des genomes :(
// + pas generique car on utilise IndividualMG With With ... et il existe aussi Individual tout court ...
// le operate est pas du tout generique
trait Selection { this: Evolution =>
  def selection (individuals: Population[G, MF]) (implicit aprng : Random): Individual[G]
}

