package org.lolczak.mt

import org.lolczak.lambda._

import scalaz._
import scalaz.effect.IO
import Scalaz._

package object test {

  /*
instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put
  state = lift . state


  trait MonadState[F[_], S] extends Monad[F] { self =>
  ////

  def state[A](a: A): F[A] = bind(init)(s => point(a))
  def constantState[A](a: A, s: => S): F[A] = bind(put(s))(_ => point(a))
  def init: F[S]
  def get: F[S]
  def gets[A](f: S => A): F[A] = bind(init)(s => point(f(s)))
  def put(s: S): F[Unit]
  def modify(f: S => S): F[Unit] = bind(init)(s => put(f(s)))

  ////

}
private trait StateTBind[S, F[_]] extends Bind[StateT[F, S, ?]] {
  implicit def F: Monad[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)
}

 */

//  import StateT.stateMonad
//  import IO.ioMonad
//
//  implicit def readerTMonadState[F[_], S, E](implicit F0: MonadState[F, S]): MonadState[ReaderT[F, E, ?], S] =
//      new MonadState[ReaderT[F, E, ?], S] {
//        override def init: ReaderT[F, E, S] = ???
//
//        override def get: ReaderT[F, E, S] = ReaderT[F, E, S](_ => F0.get)
//
//        override def put(s: S): ReaderT[F, E, Unit] = ???
//
//        override def bind[A, B](fa: ReaderT[F, E, A])(f: (A) => ReaderT[F, E, B]): ReaderT[F, E, B] = fa.flatMap(f)
//
//        //ReaderWriterStateT((_, s) => F.point((W.zero, a, s)))
//        override def point[A](a: => A): ReaderT[F, E, A] = ReaderT[F, E, A](_ => F0.point[A](a))
//      }
//
//
//  type StateType = Int
//
//  type Eval[A] = ReaderT[StateT[IO, StateType, ?], Env, A]
//
//  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(StateType, A)] = {
//    val state: StateT[IO, StateType, A] = op.run(env)
//    state.run(st)
//  }
//
//  def eval(exp: Exp): Eval[LambdaValue] = exp match {
//    case Lit(i) =>
//      for {
//        _ <- tick[Eval]
////        _ <- IO.putStrLn(s"Lit $i").liftIO
//      } yield IntVal(i)
//  }
//
//  def tick[F[_]](implicit M: MonadState[F, StateType]): F[Unit] =
//    for {
//      st <- M.get
//      _ <- M.put(st + 1)
//    } yield ()

}
