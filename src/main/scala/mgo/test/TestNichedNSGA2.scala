//package mgo.test
//
//import com.google.protobuf.wrappers.DoubleValue
//import mgo._
//import mgo.contexts._
//import freedsl.dsl._
//
//object ZDT4NichedNSGAII extends App {
//
//  import algorithm.nichednsga2._
//
//  case class Phenotype(diversity: Double, optimisation: Double)
//
//  def computePhenotype(v: Vector[Double]) = {
//    val values = zdt4.compute(v)
//    Phenotype(values(0), values(1))
//  }
//
//  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
//
//    val nsga2 = NichedNSGA2(
//      phenotype = computePhenotype,
//      fitness = (p: Phenotype) => Vector(p.optimisation),
//      niche = (p: Phenotype) => grid(Vector(0.1))(Vector(p.diversity)),
//      mu = 10,
//      lambda = 100,
//      genomeSize = 6)
//
//    nsga2.
//      until(afterGeneration(10000)).
//      trace((s, is) => println(s.generation)).
//      evolution
//  }
//
//  val (finalState, finalPopulation) =
//    NichedNSGA2.run(new util.Random(42)) { imp =>
//      import imp._
//      evolution[DSL].eval
//    }
//
//  def disp = {
//    val res = result(finalPopulation, zdt4.scale)
//    res map { case (g, p, f) => g ++ Vector(p.diversity, p.optimisation) }
//  }
//
//  println(disp.map(_.mkString(",")).mkString("\n"))
//
//}
//
