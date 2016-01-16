package org.lolczak.mt

import org.lolczak.lambda.Env

import scalaz._
import scalaz.concurrent.Task

package object skalleh {

  type StateType = Int

  type Logs = List[String]
  type Eval[A] = ReaderT[EitherT[WriterT[StateT[Task, StateType, ?], Logs, ?], ?, Failure], Env, A]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): Task[(StateType, (Logs, A \/ Failure))] = {
    val either: EitherT[WriterT[StateT[Task, StateType, ?], Logs, ?], A, Failure] = op.run(env)
    val writer: WriterT[StateT[Task, StateType, ?], Logs, A \/ Failure]           = either.run
    val state:  StateT[Task, StateType, (Logs, A \/ Failure)]                     = writer.run
    val task:   Task[(StateType, (Logs, A \/ Failure))]                           = state.run(st)
    task
  }

  case class Failure(msg: String)

}
