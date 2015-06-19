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
import collection.immutable.IntMap
import math.abs

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
import fr.iscpif.mgo.genome._
import fr.iscpif.mgo.breed._
import fr.iscpif.mgo.termination._

import fr.iscpif.mgo._

object NEATSpecification extends Properties("NEAT") {

  implicit val rng: Random = new Random()

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
  } yield ConnectionGene(innode, outnode, weight, enabled, innovation)

  implicit def arbCG[I <: Innovation](implicit a: Arbitrary[I]): Arbitrary[ConnectionGene[I]] =
    Arbitrary(connectiongene[I])

  implicit def arbInputNode: Arbitrary[InputNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield InputNode(l))
  implicit def arbOutputNode: Arbitrary[OutputNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield OutputNode(l))
  implicit def arbHiddenNode: Arbitrary[HiddenNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield HiddenNode(l))
  implicit def arbNode: Arbitrary[Node] =
    Arbitrary(
      Gen.oneOf(
        arbInputNode.arbitrary,
        arbOutputNode.arbitrary,
        arbHiddenNode.arbitrary))

  def genome[I <: Innovation](implicit a: Arbitrary[I]) = for {
    connectionGenes <- Gen.containerOf[Vector, ConnectionGene[I]](connectiongene[I])
    nodes <- Gen.containerOfN[Vector, Node](
      connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.size,
      arbNode.arbitrary)
    species <- Gen.posNum[Int]
  } yield Genome(connectionGenes, IntMap(connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.toSeq.zip(nodes): _*), species)

  def archive: Gen[Archive] = for {
    roi <- Gen.containerOf[Vector, UnnumberedInnovation](unnumberedinnovation)
    speciesInIndex <- Gen.containerOf[Vector, Genome[NumberedInnovation]](genome[NumberedInnovation])
    speciesIndexes <- Gen.containerOfN[Vector, Int](speciesInIndex.size, Gen.posNum[Int])
    lastEntirePopulationFitnesses <- Gen.containerOf[Queue, Double](arbitrary[Double])
  } yield {
    val roiunique = roi.distinct
    val roiwithnumbers = (roiunique zip (1 to roiunique.length)).map { case (innov, number) => innov.setNumber(number) }
    Archive(
      globalInnovationNumber = if (roiwithnumbers.length == 0) 0 else roiwithnumbers.map { _.number }.max,
      recordOfInnovations = roiwithnumbers,
      indexOfSpecies = IntMap.empty ++ (speciesIndexes zip speciesInIndex),
      lastEntirePopulationFitnesses = lastEntirePopulationFitnesses)
  }

  def archive(offsprings: Population[Genome[NumberedInnovation], Unit, Double]): Gen[Archive] = for {
    oldarchive <- archive
  } yield new NEATArchive {
    type P = Unit
    def inputNodes: Int = ???
    def biasNodes: Int = ???
    def minimalGenome: Genome[NumberedInnovation] = ???
    def outputNodes: Int = ???
  }.archive(oldarchive, Population.empty, offsprings)

  def individual: Gen[Individual[Genome[NumberedInnovation], Unit, Double]] = for {
    g <- genome[NumberedInnovation]
    f <- Gen.choose(-100.0, 100.0)
  } yield Individual(g, ???, f)

  def population: Gen[Population[Genome[NumberedInnovation], Unit, Double]] = for {
    indivs <- Gen.containerOf[Seq, Individual[Genome[NumberedInnovation], Unit, Double]](individual)
  } yield Population.fromIndividuals(indivs)

  def arbPop: Arbitrary[Population[Genome[NumberedInnovation], Unit, Double]] = Arbitrary(population)
  /**
   * postBreeding sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found in the archive's record of innovations are attributed its number
   */
  // property("postBreeding ") =
  //   forAll(Gen.containerOf[Seq, Genome[Innovation]](genome[Innovation])) { offsprings: Seq[Genome[Innovation]] =>
  //     forAll(archive(offsprings)) { archive =>
  //       val resgenomes: Seq[Genome[NumberedInnovation]] = new NEATBreeding {
  //         def inputNodes: Int = ???
  //         def outputNodes: Int = ???
  //         def lambda = ???
  //         def initialWeight = ???
  //         def minimalGenome = ???
  //         def mutateAddLink(
  //           genome: Genome[NumberedInnovation],
  //           population: Population[G, P, F],
  //           archive: A)(implicit rng: Random): Genome[Innovation] = ???
  //       }.postBreeding(offsprings, Population.empty, archive)
  //       val newinnovations = offsprings.zip(resgenomes)
  //         .flatMap { case (o, r) => (o.connectionGenes.map { _.innovation }) zip (r.connectionGenes.map { _.innovation }) }
  //         .filter { //filter new innovations
  //           case (origi: UnnumberedInnovation, _) => true
  //           case _ => false
  //         }
  //         .map { case (_, resi) => resi }
  //       Prop(
  //         newinnovations.forall { i =>
  //           if (i.number <= archive.globalInnovationNumber) (archive.recordOfInnovations.contains(i))
  //           else true
  //         }
  //       ) :|
  //         s"All new innovation which number are less or equal to the global innovation number are identical to one in the record of innovations" &&
  //         Prop(newinnovations.flatMap { i1 =>
  //           newinnovations.map { i2 =>
  //             if (i1.sameAs(i2)) ((i1.number == i2.number) /*:|
  //                 s"$i1 and $i2 are the same and their numbers should be the same"*/ )
  //             else true
  //           }
  //         }.foldLeft(true)(_ && _)) :|
  //         s"identical innovation should have the same innovation number" &&
  //         Prop(newinnovations.flatMap { i1 =>
  //           newinnovations.map { i2 =>
  //             if (i1.sameAs(i2)) true
  //             else (i1.number != i2.number)
  //           }
  //         }.foldLeft(true)(_ && _)) :|
  //         s"different innovations should have different innovation numbers" :|
  //         // Prop(
  //         //   newinnovations.combinations(2)
  //         //   .filter { case Seq(i1, i2) => (!i1.sameAs(i2)) && (i1.number != i2.number) }
  //         //   .toVector) :| &&
  //         s"new innovation numbers = ${newinnovations.map { _.number }}" :|
  //         s"global innovation index = ${archive.globalInnovationNumber}"

  //       // all(
  //       //   (newinnovations.filter { _.number <= archive.globalInnovationNumber }
  //       //     .forall { archive.recordOfInnovations.contains(_) }) :| s"OLD All new innovation which number are less or equal to the global innovation number are identical to one in the record of innovations",
  //       //   (newinnovations.forall { case (i1: NumberedInnovation) => newinnovations.forall { case (i2: NumberedInnovation) => if (i1.sameAs(i2)) i1.number == i2.number else i1.number != i2.number } }) :| s"OLD different innovations have different innovation numbers, and identical innovation have the same innovation number"
  //       // ) :|
  //       //   s"new innovation numbers = ${newinnovations.map { case (x: NumberedInnovation) => x.number }}" :|
  //       //   s"global innovation index = ${archive.globalInnovationNumber}"

  //     }
  //   }

  trait Xorparameters {
    def mutationAddNodeProb: Double = 0.2
    def mutationAddLinkProb: Double = 0.2
    def mutationWeightSigma: Double = 1
    def mutationWeightProb0: Double = 0.5
    def mutationDisableProb: Double = 0.2
    def mutationEnableProb: Double = 0.2
    def genDistDisjointCoeff: Double = 1.0
    def genDistExcessCoeff: Double = 1.0
    def genDistWeightDiffCoeff: Double = 0.4
    def speciesCompatibilityThreshold: Double = 3.0
    def crossoverInheritDisabledProb: Double = 0.75
    def mutationAddLinkBiasProb: Double = 0.1

    val inputNodes = 2
    val biasNodes = 1
    val outputNodes = 1

    val lambda = 5

    val maximumObjective: Double = 4.0
  }

  val xortestset: Seq[(Seq[Double], Seq[Double])] =
    Vector(
      (Vector(0.0, 0.0, 1.0), Vector(0.0)),
      (Vector(0.0, 1.0, 1.0), Vector(1.0)),
      (Vector(1.0, 0.0, 1.0), Vector(1.0)),
      (Vector(1.0, 1.0, 1.0), Vector(0.0)))

  val xorneat1 = new NEAT with NEATMinimalGenomeConnectedIO with NEATFeedforwardTopology with MaximumObjectiveReachedTermination with Xorparameters {
    def evaluateNet(
      _nodes: Int,
      _inputnodes: IndexedSeq[Int],
      _outputnodes: IndexedSeq[Int],
      _edges: Seq[(Int, Int, Double)],
      _activationfunction: Traversable[(Double, Double)] => Double,
      _state: IndexedSeq[Double])(implicit rng: Random): Double = {
      val nn = NeuralNetwork.feedforwardNetwork(
        _nodes,
        _inputnodes,
        _outputnodes,
        _edges,
        _activationfunction,
        _state)
      rng.shuffle(xortestset).map {
        case (input, output) =>
          (nn.query(input) zip output).map { case (i, o) => abs(i - o) }.sum
      }.sum - 4
    }

    def activationFunction(inputsAndWeights: Traversable[(Double, Double)]): Double = ActivationFunction.tanh(inputsAndWeights)
    def neuronInitValue: Double = 0.5
  }

  property("XORtest") = Prop({

    val res =
      xorneat1.evolve.untilConverged {
        s => println(s.generation)
      }

    true
  }
  )

  // val xorneat2 = new NEAT with NEATMinimalGenomeUnconnected with NEATFeedforwardTopology with Xorparameters {

  // }

}
