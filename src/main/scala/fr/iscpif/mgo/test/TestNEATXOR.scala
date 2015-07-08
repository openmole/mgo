/*
 * Copyright (C) 03/07/2015 Guillaume Chérel
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

package fr.iscpif.mgo.test

import java.io.{ FileWriter, PrintWriter, File }
import java.util.Locale

import fr.iscpif.mgo.{ Individual, PopulationElement, Population }
import fr.iscpif.mgo.algorithm.NEAT
import fr.iscpif.mgo.breed.{ SpeciesFitnessSharing, NEATFeedforwardTopology }
import fr.iscpif.mgo.genome.NEATMinimalGenomeUnconnected
import fr.iscpif.mgo.termination.ConditionalTermination
import fr.iscpif.mgo.tools.neuralnetwork.{ ActivationFunction, Feedforward, NeuralNetwork }

import scala.collection.immutable.Map
import scala.math._
import scala.util.Random

object TestNEATXOR {

  implicit val rng = new Random

  val outputDir = "/tmp/NEATXOR/output/"
  val outputFile = outputDir ++ "singlerun.csv"
  def outputBestNetDir = outputDir ++ "singleRunBestNet/"
  def outputBestNetExt = ".gv"

  def writeToFile(filename: String, string: String) {
    val pw = new FileWriter(filename)
    try {
      pw.write(string)
    } finally {
      pw.close()
    }
  }

  def appendToFile(filename: String, string: String) {
    val pw = new FileWriter(filename, true)
    try {
      pw.write(string)
    } finally {
      pw.close()
    }
  }

  def main(args: Array[String]): Unit = {

    val xorneat = new XORNEAT {}

    println("Creating " ++ outputDir)
    new File(outputDir).mkdirs()
    println("Creating " ++ outputBestNetDir)
    new File(outputBestNetDir).mkdirs()

    // delete previously generated graph files
    new File(outputBestNetDir).listFiles().foreach { _.delete() }

    val header = s"Generation\tPopsize\tNumSpecies\tcompatibility threshold\tbest\tavg\tworse\tnodes\tedges\t[(species,fitness,size)]\n"
    println(header)
    writeToFile(outputFile, header)

    xorneat.evolve.untilConverged { s =>
      val fitnesses: Vector[Double] = if (s.population.isEmpty) Vector(0.0) else s.population.map { elt => elt.fitness }.toVector
      val bestfitness = fitnesses.max
      val worsefitness = fitnesses.min
      val averagefitness = fitnesses.sum / fitnesses.length
      val compatibilitythreshold = s.archive.speciesCompatibilityThreshold.head

      val bestNet = xorneat.createNet(s.population.maxBy { elt => elt.fitness }.phenotype)
      val nodes = bestNet.network.nodes.length
      val edges = bestNet.network.iteredges.length

      val line = s"${s.generation}\t${s.population.content.length}\t${s.archive.indexOfSpecies.size}\t$compatibilitythreshold\t$bestfitness\t$averagefitness\t$worsefitness\t$nodes\t$edges\t${xorneat.speciesFitnessesOffsprings(s.population)}\n"
      println(line)
      appendToFile(outputFile, line)

      writeToFile(outputBestNetDir ++ "%06d".format(s.generation) ++ outputBestNetExt, xorneat.dotString(bestNet))
    }
  }
}

object TestNEATXORReplications {

  implicit val rng = new Random

  val replications = 100

  val outputDir = "/tmp/NEATXOR/output"
  val outputFile = outputDir ++ "replications.csv"
  val outputBestDir = outputDir ++ "replicationsBestNets/"
  val outputBestExt = ".gv"

  def main(args: Array[String]): Unit = {

    val xorneat = new XORNEAT {}

    new File(outputDir).mkdirs()
    new File(outputBestDir).mkdirs()

    // delete previously generated gv files
    new File(outputBestDir).listFiles().foreach { _.delete() }

    val fileoutput = new PrintWriter(outputFile)

    try {
      val header = s"Generation\tPopsize\tNumSpecies\tbest\tavg\tworse\tnodes\tedges\t[(species,fitness,size)]"
      println(header)
      fileoutput.println(header)

      (1 to replications).foreach { repli =>
        val finalstate = xorneat.evolve.untilConverged { s => }
        val fitnesses: Vector[Double] = if (finalstate.population.isEmpty) Vector(0.0) else finalstate.population.map { elt => elt.fitness }.toVector
        val bestfitness = fitnesses.max
        val worsefitness = fitnesses.min
        val averagefitness = fitnesses.sum / fitnesses.length
        val speciesInfo = xorneat.speciesFitnessesOffsprings(finalstate.population)

        val bestNet = xorneat.createNet(finalstate.population.maxBy { elt => elt.fitness }.phenotype)
        val nodes = bestNet.network.nodes.length
        val edges = bestNet.network.iteredges.length

        val line = s"${finalstate.generation}\t${finalstate.population.content.length}\t${finalstate.archive.indexOfSpecies.size}\t$bestfitness\t$averagefitness\t$worsefitness\t$nodes\t$edges\t$speciesInfo}"
        println(line)
        fileoutput.println(line)

        val filebestnetoutput = new PrintWriter(outputBestDir ++ "%06d".format(repli) ++ outputBestExt)
        try {
          filebestnetoutput.println(xorneat.dotString(bestNet))
        } finally {
          filebestnetoutput.close()
        }

      }
    } finally {
      fileoutput.close()
    }
  }
}

trait XORNEAT extends NEAT with NEATMinimalGenomeUnconnected with NEATFeedforwardTopology with ConditionalTermination with SpeciesFitnessSharing {

  def interSpeciesMatingProb: Double = 0.001

  def mutationAddNodeProb: Double = 0.02
  def mutationAddLinkProb: Double = 0.03
  def mutationAddLinkBiasProb: Double = 0.0
  def mutationWeightDriftTo0: Double = 0.1
  def mutationWeightHardMin: Double = Double.NegativeInfinity
  def mutationWeightHardMax: Double = Double.PositiveInfinity

  def mutationWeightSigma: Double = 4
  def mutationWeightProb0: Double = 0.0

  def mutationDisableProb: Double = 0.0
  def mutationEnableProb: Double = 0.0

  def genDistDisjointCoeff: Double = 1.0
  def genDistExcessCoeff: Double = 1.0
  def genDistWeightDiffCoeff: Double = 0.4

  def numberSpeciesTarget: Int = 10

  def speciesCompatibilityThreshold: Double = 3.0
  def speciesCompatibilityMod: Double = 0.0
  def speciesCompatibilityMin: Double = 0.0

  def crossoverInheritDisabledProb: Double = 0.75

  def proportionKeep: Double = 0.2

  def speciesKeptIfStagnate: Int = 2
  def stagnationTimeThreshold: Int = 0

  def useSpeciesHint = true

  val inputNodes = 2
  val biasNodes = 1
  val outputNodes = 1

  val lambda = 150

  type NN = NeuralNetwork[Double, Double, Double] with Feedforward[Double, Double]

  val trainingSet: Seq[(Seq[Double], Seq[Double])] =
    Vector(
      (Vector(0.0, 0.0, 1.0), Vector(0.0)),
      (Vector(0.0, 1.0, 1.0), Vector(1.0)),
      (Vector(1.0, 0.0, 1.0), Vector(1.0)),
      (Vector(1.0, 1.0, 1.0), Vector(0.0)))

  def createNet(
    _nodes: IndexedSeq[Double],
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, Double)],
    _activationfunction: Traversable[(Double, Double)] => Double,
    _state: IndexedSeq[Double])(implicit rng: Random): NN =
    NeuralNetwork.feedforwardSparse[Double, Double, Double](
      _nodes,
      _inputnodes,
      _outputnodes,
      _edges,
      _activationfunction: Traversable[(Double, Double)] => Double,
      _state)

  def evaluateNet(nn: NN)(implicit rng: Random): Double = {
    val score = getScore(nn)
    score.map { e: Seq[Double] => e.sum / e.length }.sum / score.length
  }

  /** returns expected and actual output difference for each output neuron and for each  */
  def getScore(
    nn: NeuralNetwork[Double, Double, Double] with Feedforward[Double, Double])(implicit rng: Random): Seq[Seq[Double]] = {
    val shuffledset = rng.shuffle(trainingSet)
    shuffledset.map {
      case (input, output) =>
        (nn.outputState(nn.query(input)) zip output).map { case (o, expected) => 1.0 - abs(expected - o) }
    }
  }

  val maxSteps = 150
  def terminated(population: Population[G, P, F])(implicit rng: Random): Boolean = {
    val best: P = population.content.maxBy { elt: PopulationElement[G, P, F] => elt.fitness }.phenotype
    val nn = createNet(best)
    getScore(nn).forall(output => output.forall { 1 - _ < 0.45 })
  }

  val enLocale = new Locale("en")

  def dotString(nn: NN): String =
    nn.network.toDot(
      "",
      { n: Double => Vector(("level", n.toString)) },
      { e: Double =>
        Vector(
          ("color",
            if (e > 0.0) "red"
            else if (e < 0.0) "blue"
            else "black"),
          ("label", "%.2f".formatLocal(enLocale, e)),
          ("penwidth", abs(e).toString))
      },
      s"""rankdir=LR
       |{rank=source ${nn.inputNeurons.mkString(" ")}}
                                                       |{rank=sink ${nn.outputNeurons.mkString(" ")}}""".stripMargin)

  def activationFunction(inputsAndWeights: Traversable[(Double, Double)]): Double = ActivationFunction.tanh(inputsAndWeights)
  def neuronInitValue: Double = 0.5

  def speciesFitnessesOffsprings(
    population: Population[G, P, F]): Seq[(Int, Double, Int)] = {
    val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
      population.toIndividuals.groupBy { indiv => indiv.genome.species }
    val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map {
      case (sp, indivs) => (sp, indivs.map {
        _.fitness
      }.sum / indivs.size)
    }.toSeq
    val sumOfSpeciesFitnesses: Double = speciesFitnesses.map {
      _._2
    }.sum
    val res: Seq[(Int, Double, Int)] = speciesFitnesses.map { case (sp, f) => (sp, f, round(f * lambda / sumOfSpeciesFitnesses).toInt) }
    res.toVector
  }
}
