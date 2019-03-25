/*
 * Copyright (C) 05/03/2019 Guillaume Chérel
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

package mgo

/**
 * Approximate Bayesian computation (ABC) methods are used to draw samples
 * approximating a posterior distribution p(θ|y) ∝ p(y|θ) p(θ) when the value
 * of the likelihood p(y|θ) is unavailable but one can sample according
 * to the likelihood, typically via simulation.
 */

package object abc {
  type Matrix = Array[Array[Double]]
}
