package mgo.test

package object bench {
  def additiveGaussianNoise(sigma: Double, f: Vector[Double] => Double) =
    (rng: util.Random, v: Vector[Double]) => Vector(f(v) + (rng.nextGaussian() * sigma))

}
