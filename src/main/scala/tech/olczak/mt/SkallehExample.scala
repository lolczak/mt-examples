package tech.olczak.mt

import tech.olczak.lambda._

import scalaz._
import scalaz.effect.IO

object SkallehExample {
  type StateType = Int

  type Logs = List[String]
  type Eval[A] = ReaderT[EitherT[WriterT[StateT[IO, StateType, ?], Logs, ?], ?, Failure], Env, A]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(StateType, (Logs, A \/ Failure))] = {
    val either: EitherT[WriterT[StateT[IO, StateType, ?], Logs, ?], A, Failure] = op.run(env)
    val writer: WriterT[StateT[IO, StateType, ?], Logs, A \/ Failure]           = either.run
    val state:  StateT[IO, StateType, (Logs, A \/ Failure)]                     = writer.run
    val task:   IO[(StateType, (Logs, A \/ Failure))]                           = state.run(st)
    task
  }

  case class Failure(msg: String)
}
