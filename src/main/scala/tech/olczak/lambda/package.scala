package tech.olczak

package object lambda {

  type Name = String

  sealed trait Exp
  case class Lit(i: Int)                extends Exp
  case class Var(name: Name)            extends Exp
  case class Add(x: Exp, y: Exp)        extends Exp
  case class Abs(name: Name, body: Exp) extends Exp
  case class App(left: Exp, right: Exp) extends Exp

  sealed trait LambdaValue
  case class IntVal(value: Int)                      extends LambdaValue
  case class FunVal(env: Env, name: Name, body: Exp) extends LambdaValue

  type Env = Map[Name, LambdaValue]

}
