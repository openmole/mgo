/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.test

import fr.iscpif.mgo.Evolution
import fr.iscpif.mgo.archive.CMAESArchive
import fr.iscpif.mgo.breed.CMAESBreeding
import fr.iscpif.mgo.elitism.{ BestAggregatedElitism, KeepOffspringElitism }
import fr.iscpif.mgo.fitness.{ MGFitness, MaxAggregation }
import fr.iscpif.mgo.genome.{ GAGenomeWithRandomValue, ClampedGenome, GAGenome }
import fr.iscpif.mgo.modifier.NoModifier
import fr.iscpif.mgo.problem.GAProblem
import fr.iscpif.mgo.termination.CounterTermination
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.linear.{ EigenDecomposition, Array2DRowRealMatrix, RealMatrix }
import org.apache.commons.math3.optim._
import org.apache.commons.math3.optim.nonlinear.scalar.{ GoalType, ObjectiveFunction }
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.CMAESOptimizer
import org.apache.commons.math3.optim.univariate.SearchInterval
import org.apache.commons.math3.random.Well1024a
import org.apache.commons.math3.util.{ FastMath, MathArrays }

import scala.util.Random
import scalax.io

object TestCMAES extends App {

  val m = new Rastrigin with Evolution with KeepOffspringElitism with GAGenomeWithRandomValue with NoModifier with MaxAggregation with CMAESBreeding with CMAESArchive with CounterTermination with ClampedGenome {
    /** Number of steps before the algorithm stops */
    override def steps: Int = 100000
    override def genomeSize: Int = 10

    /** the size of the offspring */
    override def lambda: Int = 1
    def mu = 1000
  }

  implicit val rng = new Random(46)

  val res =
    m.evolve.untilConverged {
      s =>
        println(s.generation + " " + s.individuals.size + " " + s.individuals.map(i => m.aggregate(m.fitness(i))).min)
    }.individuals

  val output = io.Resource.fromFile("/tmp/res.csv")
  for {
    r <- res
  } {
    def line = m.scale(m.values.get(r.genome)) ++ m.fitness(r)
    output.append(line.mkString(",") + "\n")
  }

  /*val rng = new Well1024a

  val f = new MultivariateFunction {
    override def value(point: Array[Double]): Double = {
      point.max
    }
  }

  val cc = new ConvergenceChecker[PointValuePair] {
    override def converged(iteration: Int, previous: PointValuePair, current: PointValuePair): Boolean = false
  }

  val initialGuess = Iterator.continually(rng.nextDouble).take(2).toArray
  val initialSigma = Iterator.continually(rng.nextDouble).take(2).toArray
  val lowerBounds = Iterator.continually(0.0).take(2).toArray
  val upperBounds = Iterator.continually(1.0).take(2).toArray

  val optimizer = new CMAESOptimizer(
    10, -1, true, 0, 10, rng, false, cc)

  val solution = optimizer.optimize(
    new ObjectiveFunction(f),
    GoalType.MINIMIZE,
    //new SearchInterval(0, 1),
    new InitialGuess(initialGuess),
    new CMAESOptimizer.PopulationSize(100),
    new CMAESOptimizer.Sigma(initialSigma),
    new SimpleBounds(lowerBounds, upperBounds),
    new MaxEval(Int.MaxValue),
    new MaxIter(Int.MaxValue))

  println(solution.getPoint.toSeq)*/

}
