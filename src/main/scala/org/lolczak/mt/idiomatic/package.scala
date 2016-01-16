package org.lolczak.mt

import org.lolczak.lambda._

import scalaz._
import scalaz.effect.IO
import Scalaz._
import IO._

package object idiomatic {

  type StateType = Int

  type Logs = List[String]
  type Eval[A] = RWST[IO, Env, Logs, StateType, A]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(Logs, A, StateType)] = {
    op.run(env, st)
  }

  def eval(exp: Exp): Eval[LambdaValue] = {
    implicit val M0 = implicitly[MonadTrans[λ[(α[_], β) => RWST[α, Env, Logs, StateType, β]]]]
    exp match {
      case Lit(i) =>
        for {
          _ <- tick[Eval]
          _ <-  M0.liftM(putStrLn(s"Lit $i"))
        } yield IntVal(i).asInstanceOf[LambdaValue]
    }
  }

  def tick[F[_]](implicit M: MonadState[F, Int]): F[Unit] =
    for {
      st <- M.get
      _  <- M.put(st + 1)
    } yield ()


  case class Failure(msg: String)

}
