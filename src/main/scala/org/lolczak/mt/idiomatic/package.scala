package org.lolczak.mt

import org.lolczak.lambda._

import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._

package object idiomatic {

  type StateType = Int

  type Logs = List[String]
  type Eval[A] = RWST[IO, Env, Logs, StateType, A]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(Logs, A, StateType)] = {
    op.run(env, st)
  }

  def eval(exp: Exp): Eval[LambdaValue] = {
    implicit val E0 = implicitly[MonadTrans[λ[(α[_], β) => RWST[α, Env, Logs, StateType, β]]]]
    import E0._
    implicit val E1 = implicitly[MonadReader[Eval, Env]]
    implicit val E2 = implicitly[MonadListen[Eval, Logs]]
    import E1._
    import E2._
    exp match {
      case Lit(i) =>
        for {
          _ <- tick[Eval]
          _ <- liftM(putStrLn(s"Lit $i"))
        } yield IntVal(i).asInstanceOf[LambdaValue]
      case Var(name) =>
        for {
          _ <- tick[Eval]
          _ <- tell(List(s"name $name"))
          env <- ask
        } yield env(name) //todo handle error
      case Add(e1, e2) =>
        for {
          _ <- tick[Eval]
          val1 <- eval(e1) //todo handle error
          val2 <- eval(e2)
          result = (val1, val2) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 + v2).asInstanceOf[LambdaValue]
          }
        } yield result
      case Abs(name, body) =>
        for {
          _ <- tick[Eval]
          env <- ask
        } yield FunVal(env, name, body).asInstanceOf[LambdaValue]
      case App(e1, e2) =>
        for {
          _ <- tick[Eval]
          val1 <- eval(e1)
          val2 <- eval(e2)
          result <- val1 match {
            case FunVal(env2, name, body) => local(_ => env2 + (name -> val2))(eval(body))
          }
        } yield result
    }
  }

  def tick[F[_]](implicit M: MonadState[F, Int]): F[Unit] =
    for {
      st <- M.get
      _ <- M.put(st + 1)
    } yield ()


  case class Failure(msg: String)

}
