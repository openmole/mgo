///*
// * Copyright (C) 09/07/2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
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
//package mgo.test
//
//import java.io.{ File, FileWriter }
//import java.util.Locale
//
//import mgo.{ Individual, PopulationElement, Population }
//import mgo.algorithm.{ HyperNEAT, NEAT }
//import mgo.breed.{ NEATSpeciesFitnessSharing, NEATFeedforwardTopology }
//import mgo.genome.NEATMinimalGenomeConnectedIO
//import mgo.termination.ConditionalTermination
//import mgo.tools.neuralnetwork.{ ActivationFunction, Feedforward, NeuralNetwork }
//
//import scala.collection.immutable.Map
//import scala.math._
//import scala.util.Random
//
//object TestHyperNEATBoxes {
//
//  implicit val rng = new Random
//
//  val outputDir = "/tmp/HyperNEATBoxes/output/"
//  val outputFile = outputDir ++ "singlerun.csv"
//  def outputBestNetDir = outputDir ++ "singleRunBestNet/"
//  def outputBestNetExt = ".gv"
//
//  def writeToFile(filename: String, string: String) {
//    val pw = new FileWriter(filename)
//    try {
//      pw.write(string)
//    } finally {
//      pw.close()
//    }
//  }
//
//  def appendToFile(filename: String, string: String) {
//    val pw = new FileWriter(filename, true)
//    try {
//      pw.write(string)
//    } finally {
//      pw.close()
//    }
//  }
//
//  def main(args: Array[String]) {
//
//    val hnb = new HyperNEATBoxes {}
//
//    println("Test set (sample)")
//    hnb.testSample(2).zipWithIndex.foreach {
//      case ((input, output, (expX, expY)), i) =>
//        println(
//          s"""---- input $i ----
//           |${matToString(input, { (x: Double) => if (x == 1.0) "#" else "." })}
//           |---- output $i: $expX, $expY ----
//           |${matToString(output, { (x: Double) => if (x == 1.0) "#" else "." })}
//           |""".stripMargin)
//    }
//
//    new File(outputDir).mkdirs()
//    new File(outputBestNetDir).mkdirs()
//
//    // delete previously generated graph files
//    new File(outputBestNetDir).listFiles().foreach { _.delete() }
//
//    val header = s"Generation\tPopsize\tNumSpecies\tcompatibility threshold\tbest\tavg\tworse\tnodes\tedges\t[(species,fitness,size)]\n"
//    println(header)
//    writeToFile(outputFile, header)
//
//    hnb.evolve.untilConverged { s =>
//      val fitnesses: Vector[Double] = if (s.population.isEmpty) Vector(0.0) else s.population.map { elt => elt.fitness }.toVector
//      val bestfitness = fitnesses.max
//      val worsefitness = fitnesses.min
//      val averagefitness = fitnesses.sum / fitnesses.length
//      val compatibilitythreshold = s.archive.speciesCompatibilityThreshold.tail.head
//
//      val bestNet = hnb.createNet(s.population.maxBy { elt => elt.fitness }.phenotype)
//      val nodes = bestNet.network.nodes.length
//      val edges = bestNet.network.iteredges.length
//
//      val line = s"${s.generation}\t${s.population.content.length}\t${s.archive.indexOfSpecies.size}\t$compatibilitythreshold\t$bestfitness\t$averagefitness\t$worsefitness\t$nodes\t$edges\t${hnb.speciesFitnessesOffsprings(s.population)}\n"
//      println(line)
//      appendToFile(outputFile, line)
//
//      writeToFile(outputBestNetDir ++ "%06d".format(s.generation) ++ outputBestNetExt, hnb.dotString(bestNet))
//      writeToFile(
//        outputBestNetDir ++ "test%06d".format(s.generation) ++ ".txt",
//        testCaseString(
//          hnb.testSample(10).map {
//            case (input, expectedoutput, (expX, expY)) =>
//              val substrate = hnb.createSubstrate(bestNet)
//              val actualoutput = substrate.outputState(substrate.query(input.flatten)).grouped(hnb.boxOutputSize).toVector
//              ((expX, expY), input, expectedoutput, actualoutput)
//          }))
//    }
//  }
//
//  val enLocale = new Locale("en")
//
//  def testCaseString(testSample: Seq[((Int, Int), Seq[Seq[Double]], Seq[Seq[Double]], Seq[Seq[Double]])]) =
//    testSample.zipWithIndex.map {
//      case (((expX, expY), input, expectedOutput, networkOutput), i) =>
//        s"""---- input $i ----
//           |${matToString(input, { (x: Double) => if (x == 1.0) "#" else "." })}
//           |---- expected output $i: $expX, $expY ----
//           |${matToString(expectedOutput, { (x: Double) => if (x == 1.0) "#" else "." })}
//           |---- network output $i ----
//           |${matToString(networkOutput, { (x: Double) => "%.2f".formatLocal(enLocale, x) })}
//           |""".stripMargin
//    }.mkString("\n")
//
//  def matToString(m: Seq[Seq[Double]], f: Double => String): String =
//    "  " ++ m(0).indices.map { _.formatted("%2d") }.mkString("") ++ "\n" ++
//      m.zipWithIndex.map {
//        case (row, i) =>
//          i.formatted("%2d ") ++ row.map {
//            f(_)
//          }.mkString(" ")
//      }.mkString("\n")
//}
//
//trait HyperNEATBoxes extends HyperNEAT with NEATFeedforwardTopology with ConditionalTermination with NEATSpeciesFitnessSharing with NEATMinimalGenomeConnectedIO {
//
//  val rng = new Random()
//
//  def interSpeciesMatingProb: Double = 0.001
//
//  def mutationAddNodeProb: Double = 0.03
//  def mutationAddLinkProb: Double = 0.1
//  def mutationAddLinkBiasProb: Double = 0.0
//  def mutationWeightDriftTo0: Double = 0.1
//  def mutationWeightHardMin: Double = -3
//  def mutationWeightHardMax: Double = 3
//
//  def mutationWeightSigma: Double = 2
//  def mutationWeightProb0: Double = 0.05
//
//  def mutationDisableProb: Double = 0.0
//  def mutationEnableProb: Double = 0.0
//
//  def genDistDisjointCoeff: Double = 1.0
//  def genDistExcessCoeff: Double = 1.0
//  def genDistWeightDiffCoeff: Double = 0.4
//
//  def numberSpeciesTarget: Int = 8
//  def speciesCompatibilityThreshold: Double = 3.0
//  def speciesCompatibilityMod: Double = 0.3
//  def speciesCompatibilityMin: Double = 0.3
//
//  def crossoverInheritDisabledProb: Double = 0.75
//
//  def proportionKeep: Double = 0.2
//
//  def speciesKeptIfStagnate: Int = 2
//  def stagnationTimeThreshold: Int = 0
//
//  def useSpeciesHint = false
//
//  def cppnOutputMin = -1.0
//  def cppnOutputMax = 1.0
//
//  def substrateExpressEdgeThreshold: Double = 0.1
//
//  val inputNodes = 4
//  val biasNodes = 1
//  val outputNodes = 1
//
//  val lambda = 100
//
//  type CPPN = NeuralNetwork[(Double, String, String), Double, Double] with Feedforward[Double, Double]
//  type NN = CPPN
//  type SUBSTRATE = NeuralNetwork[Unit, Double, Double] with Feedforward[Double, Double]
//
//  def sector(i: Int, sectorRadius: Int, wholeSize: Int): (Int, Int) = {
//    val left = max(0, i - sectorRadius)
//    val right = min(i + sectorRadius, wholeSize - 1)
//    (left, right)
//  }
//
//  def evalSubSample: Int = 10
//  def sectorRadius: Int = 2
//
//  val boxInputSize = 13
//  val boxOutputSize = 10
//
//  def testCase: (Seq[Seq[Double]], Seq[Seq[Double]], (Int, Int)) = {
//    val x = rng.nextDouble()
//    val y = rng.nextDouble()
//    val inputX = (x * boxInputSize).toInt
//    val inputY = (y * boxInputSize).toInt
//    val outputX = (x * boxOutputSize).toInt
//    val outputY = (y * boxOutputSize).toInt
//    val output = {
//      val buf =
//        Array.fill(boxOutputSize)(
//          Array.fill(boxOutputSize)(0.0)
//        )
//      buf(outputY)(outputX) = 1.0
//      buf.toVector.map { _.toVector }
//    }
//    val (sectorLeft, sectorRight) = sector((x * boxInputSize).toInt, sectorRadius, boxInputSize)
//    val (sectorBottom, sectorTop) = sector((y * boxInputSize).toInt, sectorRadius, boxInputSize)
//    val input =
//      Vector.fill(sectorBottom)(Vector.fill(boxInputSize)(0.0)) ++
//        Vector.fill(sectorTop - sectorBottom + 1)(
//          Vector.fill(sectorLeft)(0.0) ++ Vector.fill(sectorRight - sectorLeft + 1)(1.0) ++ Vector.fill(boxInputSize - 1 - sectorRight)(0.0)) ++
//          Vector.fill(boxInputSize - 1 - sectorTop)(Vector.fill(boxInputSize)(0.0))
//
//    (input, output, (outputX, outputY))
//  }
//
//  def testSample(size: Int): Seq[(Seq[Seq[Double]], Seq[Seq[Double]], (Int, Int))] = {
//    Vector.fill(size) { testCase }
//  }
//
//  def neuronInitValue: Double = 0.0
//
//  lazy val cppnActivFunctionsMap: Map[String, Traversable[(Double, Double)] => Double] =
//    Map(
//      "gaus" -> ActivationFunction.gaussian,
//      "tanh" -> ActivationFunction.tanh,
//      "logi" -> ActivationFunction.logistic,
//      "cos" -> ActivationFunction.cos,
//      "sin" -> ActivationFunction.sin,
//      "abs" -> ActivationFunction.abs,
//      "lin" -> ActivationFunction.linear)
//
//  lazy val cppnActivationFunctions: Seq[String] =
//    cppnActivFunctionsMap.keys.toVector
//
//  def createNet(
//    _nodes: IndexedSeq[Node],
//    _inputnodes: IndexedSeq[Int],
//    _outputnodes: IndexedSeq[Int],
//    _edges: Seq[(Int, Int, Double)])(implicit rng: Random): CPPN =
//    NeuralNetwork.feedforwardSparse[(Double, String, String), Double, Double](
//      _nodes.zipWithIndex.map {
//        case (n, i) =>
//          (n.level,
//            n.data,
//            if (i == _inputnodes(0)) "x1"
//            else if (i == _inputnodes(1)) "y1"
//            else if (i == _inputnodes(2)) "x2"
//            else if (i == _inputnodes(3)) "y2"
//            else ""
//          )
//      },
//      _inputnodes,
//      _outputnodes,
//      _edges,
//      _nodes.map { n => cppnActivFunctionsMap(n.data) },
//      Vector.fill(_nodes.length)(neuronInitValue))
//
//  def createSubstrate(nn: CPPN): SUBSTRATE = {
//
//    val inputNodes = (0 until boxInputSize * boxInputSize).toVector
//
//    val outputNodes = (inputNodes.length until inputNodes.length + boxOutputSize * boxOutputSize).toVector
//
//    def inputNodeIndex(x: Int, y: Int): Int = inputNodes(y * boxInputSize + x)
//    def outputNodeIndex(x: Int, y: Int): Int = outputNodes(y * boxOutputSize + x)
//
//    val edges =
//      (for {
//        xi <- 0 until boxInputSize
//        yi <- 0 until boxInputSize
//        xo <- 0 until boxOutputSize
//        yo <- 0 until boxOutputSize
//      } yield (
//        inputNodeIndex(xi, yi),
//        outputNodeIndex(xo, yo),
//        min(cppnOutputMax,
//          max(cppnOutputMin,
//            nn.outputState(nn.query(Vector(xi.toDouble, yi.toDouble, xo.toDouble, yo.toDouble)))(0)
//          )
//        ))
//      ).filter { case (_, _, w) => w > substrateExpressEdgeThreshold }
//
//    NeuralNetwork.feedforwardSparse[Unit, Double, Double](
//      Vector.fill(inputNodes.length + outputNodes.length)(()),
//      inputNodes,
//      outputNodes,
//      edges,
//      ActivationFunction.tanh,
//      Vector.fill(inputNodes.length + outputNodes.length)(neuronInitValue)
//    )
//  }
//
//  def evaluateNet(nn: NN)(implicit rng: Random): Double = {
//    val substrate = createSubstrate(nn)
//    val scores = getOutputScore(substrate)
//    scores.sum
//  }
//
//  def getOutputScore(
//    substrate: SUBSTRATE)(implicit rng: Random): Seq[Double] = {
//    testSample(evalSubSample).map {
//      case (input, output, (expectedX, expectedY)) =>
//        val (_, argmax) =
//          substrate.outputState(substrate.query(input.flatten)).zipWithIndex
//            .maxBy { case (e, i) => e }
//        val outy = argmax / boxOutputSize
//        val outx = argmax % boxOutputSize
//        val dist = sqrt(pow((outy - expectedY), 2) + pow((outx - expectedX), 2))
//        val maxdist = sqrt(2 * pow((boxOutputSize), 2))
//        (maxdist - dist) / maxdist
//    }
//  }
//
//  val maxSteps = 10000
//  def terminated(population: Population[G, P, F])(implicit rng: Random): Boolean = {
//    val best: P = population.content.maxBy { elt: PopulationElement[G, P, F] => elt.fitness }.phenotype
//    val nn = createNet(best)
//    val substrate = createSubstrate(nn)
//    getOutputScore(substrate).forall { _ > 0.95 }
//  }
//
//  val enLocale = new Locale("en")
//
//  def dotString(nn: NN): String =
//    nn.network.toDot(
//      "",
//      { n: (Double, String, String) => Vector(("label", "\"" ++ (if (n._3.isEmpty) "" else n._3 ++ ":") ++ n._2 ++ "\"")) },
//      { e: Double =>
//        Vector(
//          ("color",
//            if (e > 0.0) "red"
//            else if (e < 0.0) "blue"
//            else "black"),
//          ("label", "%.2f".formatLocal(enLocale, e)),
//          ("penwidth", abs(e).toString))
//      },
//      s"""rankdir=LR
//         |{rank=source ${nn.inputNeurons.mkString(" ")}}
//         |{rank=sink ${nn.outputNeurons.mkString(" ")}}""".stripMargin)
//
//  def speciesFitnessesOffsprings(
//    population: Population[G, P, F]): Seq[(Int, Double, Int)] = {
//    val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
//      population.toIndividuals.groupBy { indiv => indiv.genome.species }
//    val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map {
//      case (sp, indivs) => (sp, indivs.map {
//        _.fitness
//      }.sum / indivs.size)
//    }.toSeq
//    val sumOfSpeciesFitnesses: Double = speciesFitnesses.map {
//      _._2
//    }.sum
//    val res: Seq[(Int, Double, Int)] = speciesFitnesses.map { case (sp, f) => (sp, f, round(f * lambda / sumOfSpeciesFitnesses).toInt) }
//    res.toVector
//  }
//}
