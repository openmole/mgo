package mgo.tools

import breeze.linalg.{ DenseMatrix, DenseVector }

/*
 * Copyright (C) 2021 Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object Breeze {
  def arrayToDenseMatrix(rows: Int, cols: Int, array: Array[Array[Double]]): DenseMatrix[Double] =
    // we need to transpose the array first because of breeze column first representation of matrices
    DenseMatrix.create(rows, cols, array.transpose.flatten)

  def arrayToDenseMatrix(array: Array[Array[Double]]) = {
    assert(!array.isEmpty)
    DenseMatrix.create(rows = array.length, cols = array.head.length, array.flatten)
  }

  def arrayToDenseVector(array: Array[Double]) = DenseVector(array: _*)

  def matrixToArray(m: DenseMatrix[Double]): Array[Array[Double]] =
    Array.tabulate(m.rows, m.cols)((i, j) => m(i, j))

  def matrixToArray(m: DenseMatrix[Double], w: DenseVector[Int]): Array[Array[Double]] =
    matrixToArray(m).zipWithIndex.flatMap { case (v, i) => Array.fill(w(i))(v) }

  def vectorToArray(m: DenseVector[Double]): Array[Double] = m.toArray
}
