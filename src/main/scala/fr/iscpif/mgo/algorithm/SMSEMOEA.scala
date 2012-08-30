/*
 * Copyright (C) 2011 Sebastien Rey
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._

/**
 * This class implements the SMS-EMOA algorithm, as described in
 *
 * Michael Emmerich, Nicola Beume, and Boris Naujoks.
 * An EMO algorithm using the hypervolume measure as selection criterion.
 * In C. A. Coello Coello et al., Eds., Proc. Evolutionary Multi-Criterion Optimization,
 * 3rd Int'l Conf. (EMO 2005), LNCS 3410, pp. 62-76. Springer, Berlin, 2005.
 *
 * and
 *
 * Boris Naujoks, Nicola Beume, and Michael Emmerich.
 * Multi-objective optimisation using S-metric selection: Application to
 * three-dimensional solution spaces. In B. McKay et al., Eds., Proc. of the 2005
 * Congress on Evolutionary Computation (CEC 2005), Edinburgh, Band 2, pp. 1282-1289.
 * IEEE Press, Piscataway NJ, 2005.
 */
trait SMSEMOEA extends NSGAII with HypervolumeDiversity