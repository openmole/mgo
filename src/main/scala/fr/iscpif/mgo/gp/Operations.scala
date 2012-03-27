/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.gp

import java.util.Random

class Num (n : Double) extends NullaryOp {
  val companion = Num
  def eval (env : Map [String, Double]) = n
  override def toString = n.toString
}
object Num extends ExprFactory {
  def apply (n : Double) = new Num (n)
  def apply (e : Expr*)  = new Num (-777)
  val arity = 0
}

class Var (name : String) extends NullaryOp {
  val companion = Var
  def eval (env : Map [String, Double]) = env (name)
  override def toString = name
}
object Var extends ExprFactory {
  def apply (name : String) = new Var (name)
  def apply (e : Expr*) = new Var ("SHOULD_NEVER_HAPPEN")
  val arity = 0
}
class VarFactory (name : String) extends ExprFactory {
  def apply (e : Expr*) = new Var (name)
  val arity = 0
}

class Sum (val _1 : Expr, val _2 : Expr) extends BinaryOp {
  val companion = Sum
  def eval (env : Map [String, Double]) = _1.eval (env) + _2.eval (env)
  override def toString = "(" + _1 + " + " + _2 + ")"
}
object Sum extends ExprFactory {
  def apply (e : Expr*) = new Sum (e(0), e(1))
  val arity = 2
}

class Sub (val _1 : Expr, val _2 : Expr) extends BinaryOp {
  val companion = Sub
  def eval (env : Map [String, Double]) = _1.eval (env) - _2.eval (env)
  override def toString = "(" + _1 + " - " + _2 + ")"
}
object Sub extends ExprFactory {
  def apply (e : Expr*) = new Sub (e(0), e(1))
  val arity = 2
}

class Prod (val _1 : Expr, val _2 : Expr) extends BinaryOp {
  val companion = Prod
  def eval (env : Map [String, Double]) = _1.eval (env) * _2.eval (env)
  override def toString = "(" + _1 + " * " + _2 + ")"
}
object Prod extends ExprFactory {
  def apply (e : Expr*) = new Prod (e(0), e(1))
  val arity = 2
}

class Div (val _1 : Expr, val _2 : Expr) extends BinaryOp {
  val companion = Div
  def eval (env : Map [String, Double]) = _2.eval (env) match {
    case 0 => 1
    case v => _1.eval (env) / v
  }
  override def toString = "(" + _1 + " / " + _2 + ")"
}
object Div extends ExprFactory {
  val arity = 2
  def apply (e : Expr*) = new Div (e(0), e(1))
}

class Sin (val _1 : Expr) extends UnaryOp {
  val companion = Sin
  def eval (env : Map [String, Double]) = scala.math.sin (_1.eval (env))
  override def toString = "sin (" + _1 + ")"
}
object Sin extends ExprFactory {
  val arity = 1
  def apply (e : Expr*) = new Sin (e(0))
}

class Cos (val _1 : Expr) extends UnaryOp {
  val companion = Cos
  def eval (env : Map [String, Double]) = scala.math.cos (_1.eval (env))
  override def toString = "cos (" + _1 + ")"
}
object Cos extends ExprFactory {
  val arity = 1
  def apply (e : Expr*) = new Cos (e(0))
}