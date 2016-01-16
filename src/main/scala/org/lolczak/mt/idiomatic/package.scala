package org.lolczak.mt

import org.lolczak.lambda._

import scalaz._
import scalaz.effect.IO

package object idiomatic {

  type StateType = Int

  type Logs = List[String]
  type Eval[A] = EitherT[RWST[IO, Env, Logs, StateType, ?], A, Failure]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(Logs, A \/ Failure, StateType)] = {
    val rwst: RWST[IO, Env, Logs, StateType, A \/ Failure] = op.run //F[A \/ B]
    rwst.run(env, st)
  }

//  def eval(exp: Exp): Eval[LambdaValue] = exp match {
//    case Lit(i) =>
//      for {
//        _ <- tick[Eval]
////        _ <- IO.putStrLn(s"Lit $i").liftIO
//      } yield IntVal(i)
//  }
//
//  def tick[F[_]](implicit M: MonadState[F, Int]): F[Unit] =
//    for {
//      st <- M.get
//      _ <- M.put(st + 1)
//    } yield ()


  case class Failure(msg: String)

}
