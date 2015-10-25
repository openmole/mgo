///*
// * Copyright (C) 2015 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.mutation
//
//import fr.iscpif.mgo.genome.{ GA, Sigma }
//
//trait DynamicGAMutation <: DynamicMutation with GA with Sigma with MinimumSigma {
//
//  def mutations =
//    Vector(
//      BGAMutation(mutationRate = 1.0 / genomeSize, mutationRange = 0.1)(values),
//      BGAMutation(mutationRate = 0.5, mutationRange = 0.5)(values),
//      AdaptiveCauchyMutation(minimumSigma)(values, sigma)
//    )
//
//}
