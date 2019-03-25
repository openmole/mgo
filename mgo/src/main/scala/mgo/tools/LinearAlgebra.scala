/*
 * Copyright (C) 12/03/2019 Guillaume Chérel
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

package mgo.tools

import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix

object LinearAlgebra {

  def functorVectorVectorDoubleToRealMatrix(f: Vector[Vector[Double]] => Vector[Vector[Double]])(thetas: RealMatrix): RealMatrix =
    MatrixUtils.createRealMatrix(
      f(thetas.getData.map { _.toVector }.toVector).map { _.toArray }.toArray)

  def functorVectorVectorDoubleToMatrix(f: Vector[Vector[Double]] => Vector[Vector[Double]])(thetas: Array[Array[Double]]): Array[Array[Double]] =
    f(thetas.map { _.toVector }.toVector).map { _.toArray }.toArray
}
