package mgo.tools.benchmark


/**
  * Chek Cheng, R., Li, M., Tian, Y., Zhang, X., Yang, S., Jin, Y., & Yao, X. (2017). A benchmark test suite for evolutionary many-objective optimization. Complex & Intelligent Systems, 3(1), 67-81.
  */
object ManyObjective {


  /**
    * Sub function for DTLZ1 and DTLZ4
    */
  def dtlz1g(d: Int, k: Int, x: Vector[Double]): Double = ((d - k + 1) to d).map(i => math.pow(x(i - 1) - 0.5,2.0)).sum

  /**
    * MaF1 - modified inverted DTLZ1
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param x variable
    * @return
    */
  def maf1(d: Int, k: Int = 10)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF1 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF1 must have less objectives than parameters")
    val g = dtlz1g(d,k,x)
    val prods = (1 until m).map{i =>
      (1 to m - i).map(j => x(j)).product
    }
    val f1 = (1 - prods(0))*g
    val fm = x(0)*g
    Vector(f1)++prods.tail.zip(x.tail.reverse.tail.map(xi => 1-xi)).map{case (p, y) => (1 - p*y)*g}.toVector++Vector(fm)
  }


  /**
    * MaF2 - DTLZ2BZ
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param x variable
    * @return
    */
  def maf2(d: Int, k: Int = 10)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF2 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF2 must have less objectives than parameters")
    val thetai = x.map{xi => math.Pi / 2.0 * (xi / 2.0 + 0.25) }
    val gi = (1 until m).map{i =>
      val from = (m + (i - 1)*math.floor((d - m + 1.0) / m.toDouble)).toInt
      val to = (m + i*math.floor((d - m + 1.0) / m.toDouble) - 1).toInt
      (from to to).map{j => math.pow((x(j-1)/2.0 + 0.25) - 0.5,2.0)}.sum
    }
    val gm = ((m + (m - 1)*math.floor((d - m + 1.0) / m.toDouble)).toInt to d).map {j => math.pow((x(j-1)/2.0 + 0.25) - 0.5,2.0)}.sum
    val trigoprods = (2 until m).map{ i =>
      thetai.dropRight(k + i - 1).map(math.cos).product*math.sin(thetai(m - i))
    }.toVector
    val trigo = Vector(thetai.dropRight(k).map(math.cos).product)++trigoprods++Vector(math.sin(thetai(0)))
    val allgis = gi.toVector++Vector(gm)
    trigo.zip(allgis).map{case (t,g) => t*g}
  }


  /**
    * subfunction used in DTLZ3
    */
  def dtlz3g(d: Int, k: Int, x: Vector[Double]): Double =
    100.0*(k.toDouble + ((d - k + 1) to d).map{i => math.pow(x(i-1)-0.5,2.0) - math.cos(20.0*math.Pi*(x(i-1)-0.5)) }.sum)

  /**
    * subfunction used in DTLZ3 and DTLZ4
    */
  def dtlz3trigo(d: Int, k: Int, x: Vector[Double], alpha: Double = 1.0): Vector[Double] = {
    val fi = (2 until d - k + 1).map { i =>
      x.dropRight(k + i - 1).map(xi => math.cos(math.Pi / 2.0 * math.pow(xi,alpha))).product * math.sin(math.Pi / 2.0 * math.pow(x(d - k + 1 - i),alpha))
    }.toVector
    val f1 = x.dropRight(k).map(xi => math.cos(math.Pi / 2.0 * math.pow(xi,alpha))).product
    val fm = math.sin(math.Pi / 2.0 * math.pow(x(0),alpha))
    Vector(f1)++fi++Vector(fm)
  }

  /**
    * MaF3 - convex DTLZ3
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param x variable
    * @return
    */
  def maf3(d: Int, k: Int = 10)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF3 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF3 must have less objectives than parameters")
    val g = dtlz3g(d,k,x)
    val fi = dtlz3trigo(d,k,x)
    fi.dropRight(1).map(trigo => math.pow(trigo*(1+g),4.0))++Vector(math.pow(fi.last*(1+g),2.0))
  }



  /**
    * MaF4 - inverted badly scaled DTLZ3
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param a scaling factor
    * @param x variable
    * @return
    */
  def maf4(d: Int, k: Int = 10, a: Double = 2.0)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF4 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF4 must have less objectives than parameters")
    val g = dtlz3g(d,k,x)
    val fi = dtlz3trigo(d,k,x)
    (1 to m).map{i => math.pow(a,i.toDouble)}.zip(fi).map{case (apow,trigo) => apow*(1 - trigo)*(1+g)}.toVector
  }


  /**
    * MaF5 - convex badly scaled DTLZ4
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param a scaling factor
    * @param alpha exponent (within trigonometric functions)
    * @param x variable
    * @return
    */
  def maf5(d: Int, k: Int = 10, a: Double = 2.0, alpha: Double = 100.0)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF5 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF5 must have less objectives than parameters")
    val g = dtlz1g(d,k,x)
    val fi = dtlz3trigo(d,k,x,alpha=alpha)
    (1 to m).map{i => math.pow(a,i.toDouble)}.reverse.zip(fi).map{case (apow,trigo) => apow*math.pow(trigo*(1+g),4.0)}.toVector
  }


  /**
    * MaF6 - DTLZ5(I,M)
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param i dimensionality of the degenerate pareto front
    * @param x variable
    * @return
    */
  def maf6(d: Int, k: Int = 10, i: Int = 2)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF6 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF6 must have less objectives than parameters")
    assert(i<=m-1,"Incorrect Pareto front dimension in MAF6")
    val g = dtlz1g(d,k,x)
    val thetai = (1 until i).map{j => x(j-1)}.toVector ++ (i until m).map{j => (1 + 2*g*x(j-1))/(4*(1+g))}
    val trigos = (2 until m).map{j =>
      thetai.dropRight(j - 1).map(math.cos).product*math.sin(thetai(m - j))
    }.toVector
    (Vector(thetai.map(math.cos).product)++trigos++Vector(math.sin(thetai(0)))).map(trig => trig*(1 + 100.0*g))
  }


  /**
    * MaF7 - DTLZ7
    *
    * @param d number of parameters
    * @param k fixes the number of objectives such that m = d - k + 1
    * @param x variable
    * @return
    */
  def maf7(d: Int, k: Int = 20)(x: Vector[Double]): Vector[Double] = {
    val m = d - k + 1
    assert(m>0,"MAF7 must have objectives:  m = d - k + 1")
    assert(m<=d,"MAF7 must have less objectives than parameters")
    val fi = x.dropRight(k)
    val g = 1 + 9/k*x.drop(m - 1).sum
    val h = m - fi.map{f => f/(1+g)*(1 + math.sin(3.0*math.Pi*f))}.sum
    fi++Vector(h*(1+g))
  }


  /**
    * MaF8 - multi-point distance minimization problem
    *  number of parameters is always 2, number of objectives changes
    *
    * @param m number of objectives
    * @param x variable \in [-10000,10000] - always two dimensional
    * @return
    */
  def maf8(m: Int)(x: Vector[Double]): Vector[Double] = {
    // construct points at distance 1 and angle 2pi/m
    val points = (0 until m).map(i => (math.cos(2*math.Pi*i/m),math.sin(2*math.Pi*i/m)))
    points.map{case (xi,yi) =>
      math.sqrt(math.pow(xi - x(0),2.0) + math.pow(yi - x(1),2.0))
    }.toVector
  }

  




}
