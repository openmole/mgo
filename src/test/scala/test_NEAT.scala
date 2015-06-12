/*
 * Copyright (C) Guillaume Chérel 2015
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test

import util.Random

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import fr.iscpif.mgo.tools.neuralnetwork._
import fr.iscpif.mgo.algorithm.NEAT
import fr.iscpif.mgo.problem.Problem
import fr.iscpif.mgo.genome.NEATGenome._
import fr.iscpif.mgo.archive.NEATArchive.Archive
import fr.iscpif.mgo.Population
import fr.iscpif.mgo.PopulationElement

import fr.iscpif.mgo._

object NEATSpecification extends Properties("NEAT") {

  //val neat = Gen.const(new NEAT {})

  implicit val rng: Random = new Random()

  val xortestset: Gen[Seq[(Seq[Double], Seq[Double])]] =
    Generate.shuffle(
      Vector(
        (Vector(0.0, 0.0), Vector(0.0)),
        (Vector(0.0, 1.0), Vector(1.0)),
        (Vector(1.0, 0.0), Vector(1.0)),
        (Vector(1.0, 1.0), Vector(0.0)))
    )

  val node = Gen.sized { size => Gen.choose[Int](0, size) }
  val innovationNumber = Gen.sized { size => Gen.choose[Int](0, size) }
  val weight = Gen.sized { size => Gen.choose[Double](-size, size) }
  val fitness = Gen.sized { size => Gen.choose[Double](-size, size) }

  val emptyPopulation = Gen.const(Population.empty)

  val unnumberedlinkinnovation = for {
    innode <- node
    outnode <- node
  } yield UnnumberedLinkInnovation(innode, outnode)
  val numberedlinkinnovation = for {
    number <- innovationNumber
    innode <- node
    outnode <- node
  } yield NumberedLinkInnovation(number, innode, outnode)
  val unnumberednodeinnovation = for {
    innode <- node
    outnode <- node
    splittedLinkInnovationNumber <- innovationNumber
  } yield UnnumberedNodeInnovation(innode, outnode, splittedLinkInnovationNumber)
  val numberednodeinnovation = for {
    number <- innovationNumber
    innode <- node
    outnode <- node
    splittedLinkInnovationNumber <- innovationNumber
  } yield NumberedNodeInnovation(number, innode, outnode, splittedLinkInnovationNumber)
  val unnumberedinnovation = Gen.oneOf(unnumberedlinkinnovation, unnumberednodeinnovation)
  val numberedinnovation = Gen.oneOf(numberedlinkinnovation, numberednodeinnovation)
  val innovation = Gen.oneOf(numberedinnovation, unnumberedinnovation)

  val connectiongene = for {
    innode <- node
    outnode <- node
    weight <- weight
    enabled <- arbitrary[Boolean]
    innovation <- innovation
  } yield ConnectionGene(innode, outnode, weight, enabled, innovation)

  def genome = for {
    connectionGenes <- Gen.containerOf[Vector, ConnectionGene](connectiongene)
  } yield Genome(connectionGenes, connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.toSeq)

  def archive = for {
    roi <- Gen.containerOf[Vector, NumberedInnovation](numberedinnovation)
    indexOfSpecies <- Gen.containerOf[Vector, Genome](genome)
  } yield Archive(
    globalInnovationNumber = if (roi.length == 0) 0 else roi.map { _.number }.max,
    recordOfInnovations = roi,
    indexOfSpecies = indexOfSpecies)

  /**
   * postBreeding sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found in the archive's record of innovations are attributed its number
   */
  property("postBreeding ") =
    forAll(Gen.containerOf[Seq, Genome](genome)) { offsprings =>
      forAll(archive) { archive =>
        val resgenomes = new NEATBreeding {
          def crossovers = ???
          def mutations = ???
          def inputNodes: Int = ???
          def outputNodes: Int = ???
          def randomGenome(implicit rng: scala.util.Random) = ???
          def selection(population: Population[G, P, F], archive: A)(implicit rng: Random): Iterator[Individual[G, P, F]] = ???
        }.postBreeding(Population.empty, offsprings, archive)
        val newinnovations = offsprings.zip(resgenomes)
          .flatMap { case (o, r) => (o.connectionGenes.map { _.innovation }) zip (r.connectionGenes.map { _.innovation }) }
          .filter {
            case (origi: UnnumberedInnovation, resi) => true
            case _ => false
          }
          .map { case (origi, resi) => resi }
        all(
          (newinnovations.forall { case (x: NumberedInnovation) => true; case _ => false }) :| s"All new innovations are numbered",
          (newinnovations.filter { case (x: NumberedInnovation) => x.number <= archive.globalInnovationNumber }
            .forall { archive.recordOfInnovations.contains(_) }) :| s"All new innovation which number are less or equal to the global innovation number are identical to one in the record of innovations",
          (newinnovations.forall { case (i1: NumberedInnovation) => newinnovations.forall { case (i2: NumberedInnovation) => if (i1.sameAs(i2)) i1.number == i2.number else i1.number != i2.number } }) :| s"different innovations have different innovation numbers, and identical innovation have the same innovation number"
        ) :|
          s"new innovation numbers = ${newinnovations.map { case (x: NumberedInnovation) => x.number }}" :|
          s"global innovation index = ${archive.globalInnovationNumber}"

      }
    }

}
