package org.lolczak.mt

import scalaz._
import scalaz.concurrent.Task

package object skalleh {
  type Env = String

  type Queue = List[Int]

  type Logs = List[String]
  type ServiceOp[A] = ReaderT[EitherT[WriterT[StateT[Task, Queue, ?], Logs, ?], ?, Failure], Env, A]

  def runService[A](env: Env, state: Queue, op: ServiceOp[A]): Task[(Queue, (Logs, A \/ Failure))] = {
    val either: EitherT[WriterT[StateT[Task, Queue, ?], Logs, ?], A, Failure] = op.run("config")
    val writer: WriterT[StateT[Task, Queue, ?], Logs, A \/ Failure]           = either.run
    val state:  StateT[Task, Queue, (Logs, A \/ Failure)]                     = writer.run
    val task:   Task[(Queue, (Logs, A \/ Failure))]                           = state.run(List.empty[Int])
    task
  }
  case class Failure(msg: String)
}
