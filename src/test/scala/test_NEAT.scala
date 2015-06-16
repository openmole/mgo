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
import collection.immutable.Queue

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
  val innovation = Gen.oneOf(unnumberedlinkinnovation, numberedlinkinnovation, unnumberednodeinnovation, numberednodeinnovation)

  implicit lazy val arbULI: Arbitrary[UnnumberedLinkInnovation] = Arbitrary(unnumberedlinkinnovation)
  implicit lazy val arbNLI: Arbitrary[NumberedLinkInnovation] = Arbitrary(numberedlinkinnovation)
  implicit lazy val arbUNI: Arbitrary[UnnumberedNodeInnovation] = Arbitrary(unnumberednodeinnovation)
  implicit lazy val arbNNI: Arbitrary[NumberedNodeInnovation] = Arbitrary(numberednodeinnovation)

  implicit lazy val arbUI: Arbitrary[UnnumberedInnovation] = Arbitrary(unnumberedinnovation)
  implicit lazy val arbNI: Arbitrary[NumberedInnovation] = Arbitrary(numberedinnovation)

  implicit lazy val arbI: Arbitrary[Innovation] = Arbitrary(innovation)

  def connectiongene[I <: Innovation](implicit a: Arbitrary[I]) = for {
    innode <- node
    outnode <- node
    weight <- weight
    enabled <- arbitrary[Boolean]
    innovation <- arbitrary[I]
    species <- arbitrary[Int]
  } yield ConnectionGene(innode, outnode, weight, enabled, innovation, species)

  implicit def arbCG[I <: Innovation](implicit a: Arbitrary[I]): Arbitrary[ConnectionGene[I]] =
    Arbitrary(connectiongene[I])

  def genome[I <: Innovation](implicit a: Arbitrary[I]) = for {
    connectionGenes <- Gen.containerOf[Vector, ConnectionGene[I]](connectiongene[I])
  } yield Genome(connectionGenes, connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.toSeq)

  def archive: Gen[Archive] = for {
    roi <- Gen.containerOf[Vector, UnnumberedInnovation](unnumberedinnovation)
    indexOfSpecies <- Gen.containerOf[Vector, Genome[NumberedInnovation]](genome[NumberedInnovation])
    lastEntirePopulationFitnesses <- Gen.containerOf[Queue, Double](arbitrary[Double])
  } yield {
    val roiunique = roi.distinct
    val roiwithnumbers = (roiunique zip (1 to roiunique.length)).map { case (innov, number) => innov.setNumber(number) }
    Archive(
      globalInnovationNumber = if (roiwithnumbers.length == 0) 0 else roiwithnumbers.map { _.number }.max,
      recordOfInnovations = roiwithnumbers,
      indexOfSpecies = indexOfSpecies,
      lastEntirePopulationFitnesses = lastEntirePopulationFitnesses)
  }

  /**
   * postBreeding sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found in the archive's record of innovations are attributed its number
   */
  property("postBreeding ") =
    forAll(Gen.containerOf[Seq, Genome[Innovation]](genome[Innovation])) { offsprings: Seq[Genome[Innovation]] =>
      forAll(archive) { archive =>
        val resgenomes: Seq[Genome[NumberedInnovation]] = new NEATBreeding {
          def inputNodes: Int = ???
          def outputNodes: Int = ???
        }.postBreeding(Population.empty, offsprings, archive, ())
        val newinnovations = offsprings.zip(resgenomes)
          .flatMap { case (o, r) => (o.connectionGenes.map { _.innovation }) zip (r.connectionGenes.map { _.innovation }) }
          .filter { //filter new innovations
            case (origi: UnnumberedInnovation, _) => true
            case _ => false
          }
          .map { case (_, resi) => resi }
        Prop(
          newinnovations.forall { i =>
            if (i.number <= archive.globalInnovationNumber) (archive.recordOfInnovations.contains(i))
            else true
          }
        ) :|
          s"All new innovation which number are less or equal to the global innovation number are identical to one in the record of innovations" &&
          Prop(newinnovations.flatMap { i1 =>
            newinnovations.map { i2 =>
              if (i1.sameAs(i2)) ((i1.number == i2.number) /*:|
                  s"$i1 and $i2 are the same and their numbers should be the same"*/ )
              else true
            }
          }.foldLeft(true)(_ && _)) :|
          s"identical innovation should have the same innovation number" &&
          Prop(newinnovations.flatMap { i1 =>
            newinnovations.map { i2 =>
              if (i1.sameAs(i2)) true
              else (i1.number != i2.number)
            }
          }.foldLeft(true)(_ && _)) :|
          s"different innovations should have different innovation numbers" :|
          // Prop(
          //   newinnovations.combinations(2)
          //   .filter { case Seq(i1, i2) => (!i1.sameAs(i2)) && (i1.number != i2.number) }
          //   .toVector) :| &&
          s"new innovation numbers = ${newinnovations.map { _.number }}" :|
          s"global innovation index = ${archive.globalInnovationNumber}"

        // all(
        //   (newinnovations.filter { _.number <= archive.globalInnovationNumber }
        //     .forall { archive.recordOfInnovations.contains(_) }) :| s"OLD All new innovation which number are less or equal to the global innovation number are identical to one in the record of innovations",
        //   (newinnovations.forall { case (i1: NumberedInnovation) => newinnovations.forall { case (i2: NumberedInnovation) => if (i1.sameAs(i2)) i1.number == i2.number else i1.number != i2.number } }) :| s"OLD different innovations have different innovation numbers, and identical innovation have the same innovation number"
        // ) :|
        //   s"new innovation numbers = ${newinnovations.map { case (x: NumberedInnovation) => x.number }}" :|
        //   s"global innovation index = ${archive.globalInnovationNumber}"

      }
    }

}
