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

import fr.iscpif.mgo._
import fr.iscpif.mgo.algorithm.NEAT
import fr.iscpif.mgo.breed._
import fr.iscpif.mgo.genome._
import fr.iscpif.mgo.termination._
import fr.iscpif.mgo.tools.neuralnetwork._
import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}

import scala.math.abs
import scala.util.Random

object NEATSpecification extends Properties("NEAT") {

  implicit val rng: Random = new Random()
  //
  //  val node = Gen.sized { size => Gen.choose[Int](0, size) }
  //  val innovationNumber = Gen.sized { size => Gen.choose[Int](0, size) }
  //  val weight = Gen.sized { size => Gen.choose[Double](-size, size) }
  //  val fitness = Gen.sized { size => Gen.choose[Double](-size, size) }
  //
  //  implicit def arbInputNode: Arbitrary[InputNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield InputNode(l))
  //  implicit def arbOutputNode: Arbitrary[OutputNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield OutputNode(l))
  //  implicit def arbHiddenNode: Arbitrary[HiddenNode] = Arbitrary(for { l <- Gen.choose(0.0, 1.0) } yield HiddenNode(l))
  //  implicit def arbNode: Arbitrary[Node] =
  //    Arbitrary(
  //      Gen.oneOf(
  //        arbInputNode.arbitrary,
  //        arbOutputNode.arbitrary,
  //        arbHiddenNode.arbitrary))
  //
  //  val unnumberedlinkinnovation = for {
  //    innode <- node
  //    outnode <- node
  //  } yield UnnumberedLinkInnovation(innode, outnode)
  //  val numberedlinkinnovation = for {
  //    number <- innovationNumber
  //    innode <- node
  //    outnode <- node
  //  } yield NumberedLinkInnovation(number, innode, outnode)
  //  val unnumberednodeinnovation = for {
  //    innode <- node
  //    outnode <- node
  //    splittedLinkInnovationNumber <- innovationNumber
  //  } yield UnnumberedNodeInnovation(innode, outnode, splittedLinkInnovationNumber)
  //  val numberednodeinnovation = for {
  //    number <- innovationNumber
  //    innode <- node
  //    outnode <- node
  //    splittedLinkInnovationNumber <- innovationNumber
  //  } yield NumberedNodeInnovation(number, innode, outnode, splittedLinkInnovationNumber)
  //  val unnumberedinnovation = Gen.oneOf(unnumberedlinkinnovation, unnumberednodeinnovation)
  //  val numberedinnovation = Gen.oneOf(numberedlinkinnovation, numberednodeinnovation)
  //  val innovation = Gen.oneOf(unnumberedlinkinnovation, numberedlinkinnovation, unnumberednodeinnovation, numberednodeinnovation)
  //
  //  implicit lazy val arbULI: Arbitrary[UnnumberedLinkInnovation] = Arbitrary(unnumberedlinkinnovation)
  //  implicit lazy val arbNLI: Arbitrary[NumberedLinkInnovation] = Arbitrary(numberedlinkinnovation)
  //  implicit lazy val arbUNI: Arbitrary[UnnumberedNodeInnovation] = Arbitrary(unnumberednodeinnovation)
  //  implicit lazy val arbNNI: Arbitrary[NumberedNodeInnovation] = Arbitrary(numberednodeinnovation)
  //
  //  implicit lazy val arbUI: Arbitrary[UnnumberedInnovation] = Arbitrary(unnumberedinnovation)
  //  implicit lazy val arbNI: Arbitrary[NumberedInnovation] = Arbitrary(numberedinnovation)
  //
  //  implicit lazy val arbI: Arbitrary[Innovation] = Arbitrary(innovation)
  //
  //  def connectiongene[I <: Innovation](implicit a: Arbitrary[I]) = for {
  //    innode <- node
  //    outnode <- node
  //    weight <- weight
  //    enabled <- arbitrary[Boolean]
  //    innovation <- arbitrary[I]
  //  } yield ConnectionGene(innode, outnode, weight, enabled, innovation)
  //
  //  implicit def arbCG[I <: Innovation](implicit a: Arbitrary[I]): Arbitrary[ConnectionGene[I]] =
  //    Arbitrary(connectiongene[I])
  //
  //  def Genome(implicit a: Arbitrary[I]) = for {
  //    connectionGenes <- Gen.containerOf[Vector, ConnectionGene[I]](connectiongene[I])
  //    nodes <- Gen.containerOfN[Vector, Node](
  //      connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.size,
  //      arbNode.arbitrary)
  //    species <- Gen.posNum[Int]
  //  } yield Genome(
  //    connectionGenes,
  //    IntMap(connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.toSet.toSeq.zip(nodes): _*),
  //    species,
  //    connectionGenes.flatMap { cg => Vector(cg.inNode, cg.outNode) }.max)
  //
  //  implicit def arbGenomeInnovation: Arbitrary[Genome] = Arbitrary(Genome)
  //  implicit def arbGenomeNumberedInnovation: Arbitrary[Genome] = Arbitrary(Genome)
  //
  //  val archive: Gen[Archive] = for {
  //    roi <- Gen.containerOf[Vector, UnnumberedInnovation](unnumberedinnovation)
  //    speciesInIndex <- Gen.containerOf[Vector, Genome](Genome)
  //    speciesIndexes <- Gen.containerOfN[Vector, Int](speciesInIndex.size, Gen.posNum[Int])
  //    lastEntirePopulationFitnesses <- Gen.containerOf[Queue, Double](arbitrary[Double])
  //  } yield {
  //    val roiunique = roi.distinct
  //    val roiwithnumbers = (roiunique zip (1 to roiunique.length)).map { case (innov, number) => innov.setNumber(number) }
  //    Archive(
  //      globalInnovationNumber = if (roiwithnumbers.length == 0) 0 else roiwithnumbers.map { _.number }.max,
  //      recordOfInnovations = roiwithnumbers,
  //      indexOfSpecies = IntMap.empty ++ (speciesIndexes zip speciesInIndex),
  //      lastEntirePopulationFitnesses = lastEntirePopulationFitnesses)
  //  }
  //
  //  def archiveFromPop(offsprings: Population[Genome, Unit, Double]): Gen[Archive] = for {
  //    oldarchive <- archive
  //  } yield new NEATArchive {
  //    type P = Unit
  //    def inputNodes: Int = ???
  //    def biasNodes: Int = ???
  //    def minimalGenome: Genome = ???
  //    def outputNodes: Int = ???
  //  }.archive(oldarchive, Population.empty, offsprings)
  //
  //  def individual: Gen[Individual[Genome, Unit, Double]] = for {
  //    g <- Genome
  //    f <- Gen.choose(-100.0, 100.0)
  //  } yield Individual(g, ???, f)
  //
  //  def population: Gen[Population[Genome, Unit, Double]] = for {
  //    indivs <- Gen.containerOf[Seq, Individual[Genome, Unit, Double]](individual)
  //  } yield Population.fromIndividuals(indivs)
  //
  //  def arbPop: Arbitrary[Population[Genome, Unit, Double]] = Arbitrary(population)
  //
  //  /**
  //   * sets the innovation numbers of each new mutation in the offsprings such that
  //   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
  //   * * mutations that are identical to one found in the archive's record of innovations are attributed its number
  //   */
  //  property("setInnovationNumbers ") = {
  //    val testcases: Seq[(Seq[Genome], Archive, Seq[Int])] = Vector(
  //      //1
  //      (
  //        // the unnumbered innovation is different, should get a new number
  //        Vector(
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 0, 0, false, UnnumberedLinkInnovation(5, 7))
  //            ),
  //            nodes = IntMap.empty,
  //            species = 0,
  //            lastNodeId = 0
  //          )
  //        ),
  //          Archive(5, Vector(NumberedLinkInnovation(0, 3, 3)), IntMap.empty, Queue.empty),
  //          Vector(6) // expected link innovation
  //      ),
  //      (
  //        // the unnumbered innovation is found in the archive, should get the archive's number
  //        Vector(
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 0, 0, false, UnnumberedLinkInnovation(0, 1))
  //            ),
  //            nodes = IntMap.empty,
  //            species = 0,
  //            lastNodeId = 0
  //          )
  //        ),
  //          Archive(5, Vector(NumberedLinkInnovation(3, 0, 1)), IntMap.empty, Queue.empty),
  //          Vector(3) // expected link innovation
  //      ),
  //      (
  //        //two innovations
  //        Vector(
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 0, 0, false, UnnumberedLinkInnovation(0, 1)),
  //              ConnectionGene(0, 0, 0, false, UnnumberedNodeInnovation(0, 2, 4))
  //            ),
  //            nodes = IntMap.empty,
  //            species = 0,
  //            lastNodeId = 0
  //          )
  //        ),
  //          Archive(5, Vector(NumberedLinkInnovation(0, 1, 3)), IntMap.empty, Queue.empty),
  //          Vector(6, 7) // expected link innovation
  //
  //      ),
  //      (
  //        //two genomes
  //        Vector(
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 0, 0, false, UnnumberedLinkInnovation(0, 1))
  //            ),
  //            nodes = IntMap.empty,
  //            species = 0,
  //            lastNodeId = 0
  //          ),
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 0, 0, false, UnnumberedNodeInnovation(0, 2, 4))
  //            ),
  //            nodes = IntMap.empty,
  //            species = 0,
  //            lastNodeId = 0
  //          )
  //        ),
  //          Archive(5, Vector(NumberedLinkInnovation(0, 1, 3)), IntMap.empty, Queue.empty),
  //          Vector(6, 7) // expected link innovation
  //
  //      ),
  //      (
  //        //after addition of a new node
  //        Vector(
  //          Genome(
  //            connectionGenes = Vector(
  //              ConnectionGene(0, 1, 0.5, false, NumberedLinkInnovation(0, 0, 1)),
  //              ConnectionGene(0, 2, 1.0, true, UnnumberedNodeInnovation(0, 2, 0)),
  //              ConnectionGene(2, 1, 0.5, true, UnnumberedNodeInnovation(2, 1, 0))
  //
  //            ),
  //            nodes = IntMap(0 -> InputNode(), 1 -> OutputNode(), 2 -> HiddenNode(0.5)),
  //            species = 0,
  //            lastNodeId = 2
  //          )
  //        ),
  //          Archive(0, Vector.empty, IntMap.empty, Queue.empty),
  //          Vector(1, 2) // expected link innovation
  //
  //      )
  //    )
  //    all(
  //      testcases.map {
  //        case (offsprings, archive, newnumbersexpected) =>
  //          val resgenomes: Seq[Genome] = new NEATBreeding {
  //            def crossoverInheritDisabledProb: Double = ???
  //            def genDistDisjointCoeff: Double = ???
  //            def genDistExcessCoeff: Double = ???
  //            def genDistWeightDiffCoeff: Double = ???
  //            def mutationAddLinkBiasProb: Double = ???
  //            def mutationAddLinkProb: Double = 0
  //            def mutationAddNodeProb: Double = 0
  //            def mutationDisableProb: Double = ???
  //            def mutationEnableProb: Double = ???
  //            def mutationWeightProb0: Double = ???
  //            def mutationWeightSigma: Double = ???
  //            def speciesCompatibilityThreshold: Double = ???
  //            def inputNodes: Int = ???
  //            def biasNodes: Int = ???
  //            def outputNodes: Int = ???
  //            def lambda = ???
  //            def initialWeight = ???
  //            def minimalGenome = ???
  //            def mutateAddLink(
  //              genome: Genome,
  //              population: Population[G, P, F],
  //              archive: A)(implicit rng: Random): Genome = ???
  //          }.setInnovationNumbers(offsprings, archive)
  //          val newinnovresult = offsprings.zip(resgenomes)
  //            .flatMap { case (o, r) => (o.connectionGenes.map { _.innovation }) zip (r.connectionGenes.map { _.innovation }) }
  //            .filter { //filter new innovations
  //              case (origi: UnnumberedInnovation, _) => true
  //              case _ => false
  //            }
  //            .map { case (_, resi) => resi }
  //          Prop((newinnovresult zip newnumbersexpected).forall { case (a, b) => a.number == b }) :|
  //            s"offsprings: $offsprings" :|
  //            s"archive: $archive" :|
  //            s"newnumbersexpected: $newnumbersexpected" :|
  //            s"newnumberresult: $newinnovresult"
  //      }: _*)
  //  }

  trait Xorparameters {
    def interSpeciesMatingProb: Double = 0.001
    def mutationAddNodeProb: Double = 0.03
    def mutationAddLinkProb: Double = 0.05
    def mutationWeightSigma: Double = 1
    def mutationWeightProb0: Double = 0.5
    def mutationDisableProb: Double = 0.0
    def mutationEnableProb: Double = 0.0
    def genDistDisjointCoeff: Double = 1.0
    def genDistExcessCoeff: Double = 1.0
    def genDistWeightDiffCoeff: Double = 0.4
    def speciesCompatibilityThreshold: Double = 3.0
    def crossoverInheritDisabledProb: Double = 0.75
    def mutationAddLinkBiasProb: Double = 0.1
    def proportionKeep: Double = 0.2

    val inputNodes = 2
    val biasNodes = 1
    val outputNodes = 1

    val lambda = 50

    val maximumObjective: Double = 4.0
    val steps = 50
  }

  val xorneat1 = new NEAT with NEATMinimalGenomeConnectedIO with NEATFeedforwardTopology with MaximumObjectiveReachedTermination with Xorparameters {
    val xortestset: Seq[(Seq[Double], Seq[Double])] =
      Vector(
        (Vector(0.0, 0.0, 1.0), Vector(0.0)),
        (Vector(0.0, 1.0, 1.0), Vector(1.0)),
        (Vector(1.0, 0.0, 1.0), Vector(1.0)),
        (Vector(1.0, 1.0, 1.0), Vector(0.0)))

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
      //if (_edges.exists { case (u, v, _) => u == v }) throw new RuntimeException(s"LOOP! ${_edges}")
      val shuffledxor = rng.shuffle(xortestset)
      val res = shuffledxor.map {
        case (input, output) =>
          (nn.query(input) zip output).map { case (i, o) => abs(i - o) }.sum
      }.sum
      4 - res
    }

    def activationFunction(inputsAndWeights: Traversable[(Double, Double)]): Double = ActivationFunction.tanh(inputsAndWeights)
    def neuronInitValue: Double = 0.5
  }

  // property("Individuals in the same species are withing distance speciesCompatibilityThreshold of one another") = false
  // property("The number of offsprings of a species is equal to (species fitness / sum of species fitnesses) * population size") = false
  // property("Only the fittest offsprings survive") = false

  // A new node innovation creates 2 connection genes with different innovation numbers

  property("Neat learns XOR") =
    within(1) {
      Prop {

        val res =
          xorneat1.evolve.untilConverged { s =>
            println(s"Generation ${s.generation}, bestFitness = ${if (s.population.isEmpty) 0 else s.population.map { elt => elt.fitness }.max}")
            s.population.content.foreach { elt =>
              println(s"${elt.genome}")
            }
          }

        true
      }
    }

  // val xorneat2 = new NEAT with NEATMinimalGenomeUnconnected with NEATFeedforwardTopology with Xorparameters {

  // }

}
