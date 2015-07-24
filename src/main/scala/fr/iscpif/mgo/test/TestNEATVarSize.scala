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
import fr.iscpif.mgo.genome.{ NEATMinimalGenomeUnconnected, NEATMinimalGenomeConnectedIO }
import fr.iscpif.mgo.termination.ConditionalTermination
import fr.iscpif.mgo.tools.neuralnetwork.{ ActivationFunction, Feedforward, NeuralNetwork }

import fr.iscpif.mgo.tools.time

import scala.collection.immutable.Map
import scala.math._
import scala.util.Random

object TestNEATVarSize {

  implicit val rng = new Random

  val outputDir = "/tmp/NEATVarSize/output/"
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

    val xorneat = new NEATVarSize {}

    new File(outputDir).mkdirs()
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
      val compatibilitythreshold = s.archive.speciesCompatibilityThreshold.tail.head

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

trait NEATVarSize extends NEAT with NEATMinimalGenomeUnconnected with NEATFeedforwardTopology with ConditionalTermination with SpeciesFitnessSharing {

  val rng = new Random()

  def interSpeciesMatingProb: Double = 0.001

  def mutationAddNodeProb: Double = 0.02
  def mutationAddLinkProb: Double = 0.03
  def mutationAddLinkBiasProb: Double = 0.0
  def mutationWeightDriftTo0: Double = 0.1
  def mutationWeightHardMin: Double = Double.NegativeInfinity
  def mutationWeightHardMax: Double = Double.PositiveInfinity

  def mutationWeightSigma: Double = 4
  def mutationWeightProb0: Double = 0.05

  def mutationDisableProb: Double = 0.0
  def mutationEnableProb: Double = 0.0

  def genDistDisjointCoeff: Double = 1.0
  def genDistExcessCoeff: Double = 1.0
  def genDistWeightDiffCoeff: Double = 0.4

  def numberSpeciesTarget: Int = 10
  def speciesCompatibilityThreshold: Double = 3.0
  def speciesCompatibilityMod: Double = 0.3
  def speciesCompatibilityMin: Double = 0.3

  def crossoverInheritDisabledProb: Double = 0.75

  def proportionKeep: Double = 0.2

  def speciesKeptIfStagnate: Int = 2
  def stagnationTimeThreshold: Int = 0

  def useSpeciesHint = false

  val inputNodes = 100
  val biasNodes = 1
  val outputNodes = 100

  val lambda = 150

  type NN = NeuralNetwork[Double, Double, Double] with Feedforward[Double, Double]

  def sector(i: Int, sectorRadius: Int, wholeSize: Int): (Int, Int) = {
    val left = max(0, i - sectorRadius)
    val right = min(i + sectorRadius, wholeSize - 1)
    (left, right)
  }

  def sampleSize: Int = 500
  def sectorRadius: Int = 1

  val testset: Seq[(Seq[Double], Seq[Double])] =
    Vector.fill(sampleSize) {
      val x = rng.nextDouble()
      val inputPos = (x * inputNodes).toInt
      val outputPos = (x * outputNodes).toInt
      val output = Vector.fill(outputPos)(0.0) ++ Vector(1.0) ++ Vector.fill(outputNodes - 1 - outputPos)(0.0)
      val (sectorLeft, sectorRight) = sector((x * inputNodes).toInt, sectorRadius, inputNodes)
      val input = Vector.fill(sectorLeft)(0.0) ++ Vector.fill(sectorRight - sectorLeft + 1)(1.0) ++ Vector.fill(inputNodes - 1 - sectorRight)(0.0)
      (input, output)
    }

  def neuronInitValue: Double = 0.5

  def createNet(
    _nodes: IndexedSeq[Node],
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, Double)])(implicit rng: Random): NN =
    NeuralNetwork.feedforwardSparse[Double, Double, Double](
      _nodes.map { _.level },
      _inputnodes,
      _outputnodes,
      _edges,
      ActivationFunction.tanh,
      Vector.fill(_nodes.length)(neuronInitValue))

  def evaluateNet(nn: NN)(implicit rng: Random): Double = {
    val diff = getOutputScore(nn)
    diff.map { e: Seq[Double] => e.sum / e.length }.sum / diff.length
  }

  /** returns expected and actual output difference for each output neuron and for each  */
  def getOutputScore(
    nn: NeuralNetwork[Double, Double, Double] with Feedforward[Double, Double])(implicit rng: Random): Seq[Seq[Double]] = {
    testset.map {
      case (input, output) =>
        (nn.outputState(nn.query(input)) zip output).map { case (o, expected) => 1 - abs(expected - o) }
    }
  }

  val maxSteps = 150
  def terminated(population: Population[G, P, F])(implicit rng: Random): Boolean = {
    val best: P = population.content.maxBy { elt: PopulationElement[G, P, F] => elt.fitness }.phenotype
    val nn = createNet(best)
    getOutputScore(nn).forall(output => output.forall { _ > 0.5 })
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
