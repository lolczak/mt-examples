package org.lolczak

package object lambda {

  type Name = String

  sealed trait Exp
  case class Lit(i: Int)                extends Exp
  case class Var(name: Name)            extends Exp
  case class Plus(x: Exp, y: Exp)       extends Exp
  case class Abs(name: Name, body: Exp) extends Exp
  case class App(left: Exp, right: Exp) extends Exp

  sealed trait Value
  case class IntVal(value: Int)                      extends Value
  case class FunVal(env: Env, name: Name, body: Exp) extends Value

  type Env = Map[Name, Value]

}
