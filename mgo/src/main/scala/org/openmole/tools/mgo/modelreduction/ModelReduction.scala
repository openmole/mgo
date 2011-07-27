/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

//package org.openmole.tools.mgo.modelreduction
//import java.util.Random
//import org.openmole.tools.mgo.coevolution.Coevolution
//import org.openmole.tools.mgo.coevolution.Ga
//import org.openmole.tools.mgo.coevolution.Gp
//import org.openmole.tools.mgo.gp.ExprGenerator._
//import org.openmole.tools.mgo.gp._
//import scala.math._
//
//class SoftMutation extends Operator [IndexedSeq [Double]]{
//  import scala.math._
//  def generateGenome (pop : Iterable [Individual [IndexedSeq [Double]]])
//                     (implicit rng : java.util.Random) = {
//    val indi = pop.toArray.apply (rng.nextInt (pop.size))
//      
//    val sigma = 0.1*tanh(indi.fit/10)
//    
//    indi.genome map (x => max (min (x + sigma * rng.nextGaussian, 1), 0))
//  }
//}
//
//class CrossingOver extends Operator [IndexedSeq [Double]]{
//  def generateGenome(pop: Iterable [Individual [IndexedSeq [Double]]]) 
//                    (implicit rng : Random) : IndexedSeq [Double] = {
//    val g1 = pop.toArray.apply (rng.nextInt(pop.size)).genome
//    val g2 = pop.toArray.apply (rng.nextInt(pop.size)).genome
//    
//    IndexedSeq.tabulate (g1.size) (i => if (rng.nextBoolean) g1 (i) else g2 (i))
//  }
//}
//
//class Average extends Operator [IndexedSeq [Double]] {
//  def generateGenome (pop: Iterable [Individual [IndexedSeq [Double]]]) 
//                     (implicit rng : Random) : IndexedSeq [Double] = {
//    val g1 = pop.toArray.apply (rng.nextInt(pop.size)).genome
//    val g2 = pop.toArray.apply (rng.nextInt(pop.size)).genome
//      
//    val pds = rng.nextDouble
//      
//    IndexedSeq.tabulate (g1.size) (i => (pds*g1 (i) + (1 - pds) * g2 (i)) / 2)
//  }
//}
//
//class ChangeValue extends Operator [IndexedSeq [Double]] {
// // println("ChangeValue")
//  def generateGenome (pop: Iterable [Individual [IndexedSeq [Double]]]) 
//                     (implicit rng : Random) : IndexedSeq [Double] = {
//   //Tirage aléatoire d'un genome dans la population
//   val g = pop.toArray.apply (rng.nextInt(pop.size)).genome
//    
//   //Tirage aleatoire d'un double entre 0 et 100
//    val newValue = rng.nextDouble
//   // println("newValue " + newValue)
//    g.updated (rng.nextInt (g.size), newValue)
//  }
//}
//
///**
// * Replaces a randomly chosen subtree with a randomly created terminal
// */
//class ShrinkMutation2 (terms : IndexedSeq [ExprFactory]) 
//extends Operator [(Int, Expr)] {
//  def generateGenome (pop : Iterable [Individual [(Int, Expr)]])
//                     (implicit rng : java.util.Random) : (Int, Expr) = {
//    val rpl = terms (rng.nextInt (terms.size))
//    val g = pop.toArray.apply (rng.nextInt (pop.size)).genome
//    (g._1, replaceRandomSubtreeWith (g._2, rpl.build (Nil)))
//  }
//}
//
///*
// * Creates a new o\ufb00spring individual which is copy of a randomly chosen 
// * subtree of the parent.
// */
//object HoistMutation2 extends Operator [(Int, Expr)] {
//  def generateGenome (pop : Iterable [Individual [(Int, Expr)]]) 
//                     (implicit rng : java.util.Random) : (Int, Expr) = {
//    val g = pop.toArray.apply (rng.nextInt (pop.size)).genome
//    (g._1, chooseRandomSubtree (g._2))
//  }
//}
//
///*
// * Replaces a randomly selected subtree with another randomly created subtree
// */
//class SubtreeMutation2 (terms : IndexedSeq [ExprFactory], funs : IndexedSeq [ExprFactory]) 
//extends Operator [(Int, Expr)] {
//  def generateGenome (pop : Iterable [Individual [(Int, Expr)]]) 
//                     (implicit rng : java.util.Random) : (Int, Expr) = {
//    val g = pop.toArray.apply (rng.nextInt (1)) genome
//    val i = rng.nextInt (g._2.size)
//    (g._1, g._2.replaceSubtreeWith(i, genExpr(funs, terms, 
//                                    g._2.depth - g._2.getDepth (i), "full")))
//  }
//}
//
///**
// * Taking two trees, replace a random subtree in the most shallow by a random
// * subtree at same depth in the deeper.
// */
//object CrossingOver2 extends Operator [(Int, Expr)] {
//  def generateGenome (pop : Iterable [Individual [(Int, Expr)]]) 
//                     (implicit rng : java.util.Random) : (Int, Expr) = {
//    val (x1, t1) = pop.toArray.apply (rng.nextInt (pop.size)).genome
//    var (x2, t2) = pop.toArray.apply (rng.nextInt (pop.size)).genome
//    while (x2 != x1) {
//      var (x2, t2) = pop.toArray.apply (rng.nextInt (pop.size)).genome
//    }
//    val (deeper, shallow) = 
//      if (t1.depth >= t2.depth) (t1, t2) else (t2, t1)
//    val i = rng.nextInt (shallow.size)
//    val depth = shallow.getDepth(i)
//    val subtrees = deeper.subtreesAtDepth(depth)
//    (x1, shallow.replaceSubtreeWith(i, subtrees (rng.nextInt (subtrees.size))))
//  }
//}
//
//
//trait ModelReduction {
//  implicit val rnd : Random
//  
//  val inputsRange  : List [(Double, Double)]
//  
//  val dynamique : Model
//  
//  /**
//   * Experimental data
//   * +------+-----+-----+-----+------+------+-----+
//   * | time | in1 | in2 | ... | out1 | out2 | ... |
//   * +------+-----+-----+-----+------+------+-----+
//   * |  t1  | x11 | x21 | ... | y11  | y12  | ... |
//   * |  t1  | x11 | x21 | ... | y11  | y12  | ... |
//   * |  t2  | x11 | x21 | ... | y11  | y12  | ... |
//   * | ...  | ... | ... | ... | ...  | ...  | ... |
//   */
//  val data : IndexedSeq [IndexedSeq [Double]]
//  val inputs : IndexedSeq [Sample]
//  val outputs : IndexedSeq [Sample]
//  
//  /**
//   * Return a map of time -> data
//   */
//  def dataByTime : Map [Double, IndexedSeq [IndexedSeq [Double]]]= 
//    data groupBy (_(0)) map (x => x._1 -> (x._2 map (_.tail)))
//  
//  val parameters : IndexedSeq [String]
//  val gaOperators : List [Operator [IndexedSeq [Double]]] =
//    List (new SoftMutation, new Average, new CrossingOver, new ChangeValue)
//  val gaPopSize = 512
//  def gaFitness (g : IndexedSeq [Double]) : Double = {
//    val results : IndexedSeq [Sample] = dynamique (g, inputs)
//    (results zip outputs) map (x => 
//      ((x._1.values zip x._2.values) map (x => pow (x._1 - x._2, 2))).sum) sum
//  }
//  
//  val ga = new Ga [IndexedSeq [Double]] (parameters, gaPopSize, gaFitness, 
//                                         gaOperators)
//  
//  val gpPopSize = 128
//  def gpFitness (g : (Int, Expr), 
//                 optima : Iterable [IndexedSeq [Double]]) = {
//    optima.map (t => {
//      val env = (1 to optima.size-1 map ("param"+_) zip (
//                (t take g._1) ++ (t takeRight t.size-(g._1+1)))).toMap
//      pow (t (g._1) - g._2.eval (env), 2)
//    }).sum
//  }
//                 
//  val vars  : Array [Var] = 1 to parameters.size-1 map (i => Var ("param"+i)) toArray
//  val funs  : Array [ExprFactory] = Array (Sum, Sub, Prod)
//  val terms : Array [ExprFactory] = vars ++ Array (new IntNum (-100, 100))
//  val gpOperators : List [Operator [(Int, Expr)]] = 
//    List (CrossingOver2, HoistMutation2, new ShrinkMutation2 (terms), 
//          new SubtreeMutation2 (terms, funs))
//  val gp = new Gp [(Int, Expr)] (gpPopSize, gpFitness, gpOperators)
//  
//  def gaSelect (pop : List [Individual [IndexedSeq [Double]]]) : List [IndexedSeq [Double]]
//  def gpSelect (pop : List [Individual [(Int, Expr)]]) : List [Operator [IndexedSeq [Double]]]
//  
//  val coev = new Coevolution [IndexedSeq [Double], (Int, Expr)] (ga, gp, gaSelect,
//                                                          gpSelect, gpFitness)
//}
