package mgo.tools

/*
 * Copyright (C) 2026 Romain Reuillon
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

object ReservoirSampling:
  def empty(k: Int) = ReservoirSampling(k, IArray.empty)

  extension (reservoir: ReservoirSampling)
    def samples = reservoir.data.map(_._1)
    def merge(other: ReservoirSampling): ReservoirSampling =
      val merged = (reservoir.data ++ other.data).sortBy(_._2).take(reservoir.k)
      reservoir.copy(data = merged)

    def addAll(a: Seq[Double], rnd: scala.util.Random): ReservoirSampling =
      val merged =
        val wa = IArray.fill(a.size)(rnd.nextLong())
        (reservoir.data ++ (a zip wa)).sortBy(_._2).take(reservoir.k)

      reservoir.copy(data = merged)

final case class ReservoirSampling(k: Int, data: IArray[(Double, Long)])
