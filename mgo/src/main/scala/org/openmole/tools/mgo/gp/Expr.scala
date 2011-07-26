/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.gp
import java.util.Random
import org.openmole.tools.mgo.AbstractGenome

case class Var  (x : String) extends Expr
class PowVarsInt (vars : List [Var], min : Int, max : Int) extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr =
    Pow (vars (aprng.nextInt (vars.size)), Num (min + aprng.nextInt (max - min)))
  def arity = 0
}

case class Num  (n : Double) extends Expr
object Num extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr =
    Num (aprng.nextDouble)
  def arity = 0
}
class IntNum (min : Int, max : Int) extends ExprFactory {
  def arity = 0
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr =
    Num (min + aprng.nextInt (max - min))
}

case class Sum  (e1 : Expr, e2 : Expr) extends Expr
object Sum extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = 
    Sum (e (0), e (1))
  def arity = 2
}

case class Sub  (e1 : Expr, e2 : Expr) extends Expr
object Sub extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = 
    Sub (e (0), e (1))
  def arity = 2
}

case class Prod (e1 : Expr, e2 : Expr) extends Expr
object Prod extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = 
    Prod (e (0), e (1))
  def arity = 2
}

case class Div  (e1 : Expr, e2 : Expr) extends Expr
object Div extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = 
    Div (e (0), e (1))
  def arity = 2
}

case class Pow  (e1 : Expr, e2 : Expr) extends Expr
object Pow extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = 
    Pow (e (0), e (1))
  def arity = 2
}

case class Log  (e : Expr) extends Expr
object Log extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = Log (e (0))
  def arity = 1
}

case class Exp  (e : Expr) extends Expr
object Exp extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = Exp (e (0))
  def arity = 1
}

case class Sin  (e : Expr) extends Expr
object Sin extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = Sin (e (0))
  def arity = 1
}

case class Cos  (e : Expr) extends Expr
object Cos extends ExprFactory {
  def buildGenome (e : List [Expr]) (implicit aprng : Random) : Expr = new Cos (e (0))
  def arity = 1
}

trait Expr extends AbstractGenome {
  lazy val subtrees : List [Expr] = this match {
     case Num (n)       => Nil
     case Var (x)       => Nil
     case Sum (e1, e2)  => List (e1, e2)
     case Sub (e1, e2)  => List (e1, e2)
     case Prod (e1, e2) => List (e1, e2)
     case Div (e1, e2)  => List (e1, e2)
     case Pow (e1, e2)  => List (e1, e2)
     case Log (e)       => List (e)
     case Exp (e)       => List (e)
     case Sin (e)       => List (e)
     case Cos (e)       => List (e)
   }
  
  def subtreesAtDepth (i : Int) : List [Expr] =
    if (i == 0) List (this)
    else this match {
     case Num (n)       => Nil
     case Var (x)       => Nil
     case Sum (e1, e2)  => e1.subtreesAtDepth (i-1) ::: e2.subtreesAtDepth(i-1)
     case Sub (e1, e2)  => e1.subtreesAtDepth (i-1) ::: e2.subtreesAtDepth(i-1)
     case Prod (e1, e2) => e1.subtreesAtDepth (i-1) ::: e2.subtreesAtDepth(i-1)
     case Div (e1, e2)  => e1.subtreesAtDepth (i-1) ::: e2.subtreesAtDepth(i-1)
     case Pow (e1, e2)  => e1.subtreesAtDepth (i-1) ::: e2.subtreesAtDepth(i-1)
     case Log (e)       => e.subtreesAtDepth(i-1)
     case Exp (e)       => e.subtreesAtDepth(i-1)
     case Sin (e)       => e.subtreesAtDepth(i-1)
     case Cos (e)       => e.subtreesAtDepth(i-1)
    }
  
  lazy val size : Int = this match {
     case Num (n)       => 1
     case Var (x)       => 1
     case Sum (e1, e2)  => e1.size + e2.size + 1
     case Sub (e1, e2)  => e1.size + e2.size + 1
     case Prod (e1, e2) => e1.size + e2.size + 1
     case Div (e1, e2)  => e1.size + e2.size + 1
     case Pow (e1, e2)  => e1.size + e2.size + 1
     case Log (e)       => e.size + 1
     case Exp (e)       => e.size + 1
     case Sin (e)       => e.size + 1
     case Cos (e)       => e.size + 1
  }
  
  lazy val depth : Int = this match {
     case Num (n)       => 0
     case Var (x)       => 0
     case Sum (e1, e2)  => 1 + scala.math.max (e1.depth, e2.depth)
     case Sub (e1, e2)  => 1 + scala.math.max (e1.depth, e2.depth)
     case Prod (e1, e2) => 1 + scala.math.max (e1.depth, e2.depth)
     case Div  (e1, e2) => 1 + scala.math.max (e1.depth, e2.depth)
     case Pow (e1, e2)  => 1 + scala.math.max (e1.depth, e2.depth)
     case Log (e)       => e.depth + 1
     case Exp (e)       => e.depth + 1
     case Sin (e)       => e.depth + 1
     case Cos (e)       => e.depth + 1
  }
  
  def simplify : Expr = this match {
    case Sum  (Num (i1), Num (i2)) => Num (i1 + i2)
    case Sum  (e1, e2) => Sum (e1.simplify, e2.simplify)
    case Sub (Num (i1), Num (i2)) => Num (i1 - i2)
    case Sub (Var (s1), Var (s2)) if (s1 == s2) => Num (0)
    case Sub (e1, e2) => Sub (e1.simplify, e2.simplify)
    case Prod (Num (i1), Num (i2)) => Num (i1 * i2)
    case Prod (e1, e2) => Prod (e1.simplify, e2.simplify)
    case Div  (Num (i1), Num (i2)) => if (i2 == 0) Num (1) else Num (i1/i2)
    case Div  (Var (s1), Var (s2)) if (s1 == s2) => Num (1)
    case Div  (e1, e2) => Div (e1.simplify, e2.simplify)   
    case Pow  (Num (i1), Num (i2)) => Num (scala.math.pow (i1, i2))
    case Pow  (e1, e2) => Pow (e1.simplify, e2.simplify)   
    case Log  (Num (i))            => Num (scala.math.log (i))
    case Log  (e) => Log (e.simplify)   
    case Exp  (Num (i))            => Num (scala.math.exp (i))
    case Exp  (e) => Exp (e.simplify)   
    case Sin  (Num (i))            => Num (scala.math.sin (i))
    case Sin  (e) => Sin (e.simplify)   
    case Cos  (Num (i))            => Num (scala.math.cos (i))
    case Cos  (e) => Cos (e.simplify)
    case e => if (this == e) e else e.simplify
  }
  
  /** Return the depth of the node at index i **/
  def getDepth (i : Int) : Int = 
    if (i == 0) 0
    else this match {
      case Sum  (e1, e2) => 
        if (i <= e1.size) 1 + e1.getDepth (i-1)
        else 1 + e2.getDepth (i-e1.size-1)
      case Sub  (e1, e2) => 
        if (i <= e1.size) 1 + e1.getDepth (i-1)
        else 1 + e2.getDepth (i-e1.size-1)
      case Prod (e1, e2) => 
        if (i <= e1.size) 1 + e1.getDepth (i-1)
        else 1 + e2.getDepth (i-e1.size-1)
      case Div  (e1, e2) => 
        if (i <= e1.size) 1 + e1.getDepth (i-1)
        else 1 + e2.getDepth (i-e1.size-1)
      case Pow  (e1, e2) =>
        if (i <= e1.size) 1 + e1.getDepth (i-1)
        else 1 + e2.getDepth (i-e1.size-1)
      case Log  (e)      => 1 + e.getDepth (i-1)
      case Exp  (e)      => 1 + e.getDepth (i-1)
      case Sin  (e)      => 1 + e.getDepth (i-1)
      case Cos  (e)      => 1 + e.getDepth (i-1)
    }

  def getSubtree (i : Int) : Expr = {
    if (i < 0 || i >= this.size) error (this + " : No such node at index " + i) 
    if (i == 0) this 
    else this match {
      case Sum  (e1, e2) => 
        if (i <= e1.size) e1.getSubtree (i-1)
        else e2.getSubtree (i-e1.size-1)
      case Sub  (e1, e2) => 
        if (i <= e1.size) e1.getSubtree (i-1)
        else e2.getSubtree (i-e1.size-1)
      case Prod (e1, e2) => 
        if (i <= e1.size) e1.getSubtree (i-1)
        else e2.getSubtree (i-e1.size-1)
      case Div (e1, e2) => 
        if (i <= e1.size) e1.getSubtree (i-1)
        else e2.getSubtree (i-e1.size-1)
      case Pow  (e1, e2) =>
        if (i <= e1.size) e1.getSubtree (i-1)
        else e2.getSubtree (i-e1.size-1)
      case Log  (e)      => e.getSubtree (i-1)
      case Exp  (e)      => e.getSubtree (i-1)
      case Sin  (e)      => e.getSubtree (i-1)
      case Cos  (e)      => e.getSubtree (i-1)
    }
  }
  
  def replaceSubtreeWith (i : Int, by : Expr) : Expr = {
    if (i < 0 || i >= this.size) error (this + " : No such node at index " + i) 
    if (i == 0) by
    else this match {
        case Sum  (e1, e2) => 
          if (i <= e1.size) Sum (e1.replaceSubtreeWith (i-1, by), e2)
          else Sum (e1, e2.replaceSubtreeWith (i-e1.size-1, by))
        case Sub  (e1, e2) => 
          if (i <= e1.size) Sub (e1.replaceSubtreeWith (i-1, by), e2)
          else Sub (e1, e2.replaceSubtreeWith (i-e1.size-1, by))
        case Prod (e1, e2) => 
          if (i <= e1.size) Prod (e1.replaceSubtreeWith (i-1, by), e2)
          else Prod (e1, e2.replaceSubtreeWith (i-e1.size-1, by))
        case Div (e1, e2) => 
          if (i <= e1.size) Div (e1.replaceSubtreeWith (i-1, by), e2)
          else Div (e1, e2.replaceSubtreeWith (i-e1.size-1, by))
        case Pow  (e1, e2) =>
          if (i <= e1.size) Pow (e1.replaceSubtreeWith (i-1, by), e2)
          else Pow (e1, e2.replaceSubtreeWith (i-e1.size-1, by))
        case Log  (e)      => Log (by)
        case Exp  (e)      => Exp (by)
        case Sin  (e)      => Sin (by)
        case Cos  (e)      => Cos (by)
    }
  }

  def arity : Int = this match {
     case Num (n)       => 0
     case Var (x)       => 0
     case Sum (e1, e2)  => 2
     case Sub (e1, e2) => 2
     case Prod (e1, e2) => 2
     case Div  (e1, e2) => 2
     case Pow (e1, e2)  => 2
     case Log (e)       => 1
     case Exp (e)       => 1
     case Sin (e)       => 1
     case Cos (e)       => 1
  }

  def eval (env : Map [String, Double]) : Double = this match {
     case Num (n)       => n
     case Var (x)       => env (x)
     case Sum (e1, e2)  => (e1.eval (env), e2.eval (env)) match {
         case (0, v)   => v
         case (v, 0)   => v
         case (v1, v2) => v1 + v2
     }
     case Sub (e1, e2)  => (e1.eval (env), e2.eval (env)) match {
        case (0 , v)  => v
        case (v , 0)  => v
        case (v1, v2) => v1 - v2
     }
     case Prod (e1, e2) => (e1.eval (env), e2.eval (env)) match {
         case (0 , _) | (_ , 0) => 0
         case (1 , v)   => v
         case (v , 1)   => v
         case (v1, v2) => v1 * v2
     }
     case Div  (e1, e2) => (e1.eval (env), e2.eval (env)) match {
         case (_ , 0)  => 1.0
         case (v , 1)  => v
         case (0 , _)  => 0
         case (v1, v2) => v1 / v2
     }
     case Pow (e1, e2)  => (e1.eval (env), e2.eval (env)) match {
         case (_ , 0) | (1 , _) => 1
         case (v , 1)  => v
         case (0 , _)  => 0
         case (v1, v2) => scala.math.pow (v1, v2)
     }
     case Log (e)       => scala.math.log (e.eval (env))
     case Exp (e)       => scala.math.exp (e.eval (env))
     case Sin (e)       => scala.math.sin (e.eval (env))
     case Cos (e)       => scala.math.cos (e.eval (env))
   }
   
  def pretty : String = this match {
     case Num (n)       => n.toString
     case Var (x)       => x
     case Sum (e1, e2)  => "(" + e1.pretty + "+" + e2.pretty + ")"
     case Sub (e1, e2)  => "(" + e1.pretty + "-" + e2.pretty + ")"
     case Prod (e1, e2) => "(" + e1.pretty + "*" + e2.pretty + ")"
     case Div  (e1, e2) => "(" + e1.pretty + "/" + e2.pretty + ")"
     case Pow (e1, e2)  => "(" + e1.pretty + "^" + e2.pretty + ")"
     case Log (e)       => "log(" + e.pretty + ")"
     case Exp (e)       => "exp(" + e.pretty + ")"
     case Sin (e)       => "sin(" + e.pretty + ")"
     case Cos (e)       => "cos(" + e.pretty + ")"
   }
}

case class Sum2 (override val e1 : Expr, override val e2 : Expr) extends Sum (e1, e2) {
  override def eval (env : Map [String, Double]) : Double = this match {
    case Sum2 (e1, e2) => e1.eval (env) + e2.eval (env) + 2
    case _ => super.eval (env)
  }
}

case class Sum3 (override val e1 : Expr, override val e2 : Expr) extends Sum (e1, e2) {
  override def eval (env : Map [String, Double]) : Double = this match {
    case Sum3 (e1, e2) => e1.eval (env) + e2.eval (env) + 3
    case _ => super.eval (env)
  }
}