/*
 * Copyright (C) 18/09/2015 Guillaume Chérel
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
package fr.iscpif.mgo.genome

import fr.iscpif.mgo.genome.NEATGenome.ConnectionGene

import scala.annotation.tailrec

trait NEATGenomesAlign {

  /**Enum object for alignment state*/
  object AlignmentInfo extends Enumeration {
    type AlignmentInfo = Value
    val Aligned, Excess, Disjoint = Value
  }

  import AlignmentInfo._

  /**
   * Returns a list of aligned connection genes. The first two elements of each tuple give aligned genes, (or None for unmatching genes) and the third is set to 0 when the genes are
   * aligned, 1 for an excess genes, and 2 for disjoint genes
   */
  @tailrec final def alignGenomes(
    cg1: Seq[ConnectionGene],
    cg2: Seq[ConnectionGene],
    acc: List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)] = List.empty): List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)] = {
    if (cg1.isEmpty && cg2.isEmpty)
      acc
    else if (cg1.isEmpty)
      alignGenomes(Seq.empty, cg2.tail,
        (None, Some(cg2.head), Disjoint) :: acc)
    else if (cg2.isEmpty)
      alignGenomes(cg1.tail, Seq.empty,
        (Some(cg1.head), None, Disjoint) :: acc)
    else if (cg1.head.innovation == cg2.head.innovation)
      alignGenomes(cg1.tail, cg2.tail,
        (Some(cg1.head), Some(cg2.head), Aligned) :: acc)
    else if (cg1.head.innovation < cg2.head.innovation)
      alignGenomes(cg1.tail, cg2,
        (Some(cg1.head), None, Excess) :: acc)
    else
      alignGenomes(cg1, cg2.tail,
        (None, Some(cg2.head), Excess) :: acc)
  }
}
