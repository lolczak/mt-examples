package org.lolczak.mt

import org.lolczak.lambda._

import scalaz._
import Scalaz._
import scalaz.effect.IO

package object skalleh {

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

//  def tick[F[_]](implicit M: MonadState[F, Int]): F[Unit] =
//    for {
//      st <- M.get
//      _  <- M.put(st + 1)
//    } yield ()
//
//  def eval(exp: Exp): Eval[LambdaValue] = exp match {
//    case Lit(i) =>
//      for {
//        _ <- tick[Eval]
//        _ <- IO.putStrLn(s"Lit $i").liftIO
//      } yield IntVal(i)
//    case Var(name) =>
//      for {
//        _ <- tick
//
//      }
//    case Plus(x,y) =>
//    case Abs(name, exp) =>
//    case App(e1, e2) =>
//
//  }

}


//trait MonadStateInstances {

//  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]): MonadState[StateT[F, S, ?], S] =
//    new StateTMonadState[S, F] {
//      implicit def F: Monad[F] = F0
//    }
  /*
  instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state
   */

//  implicit def readerTMonadState[F[_], S, E](implicit F0: MonadState[F, S]): MonadState[ReaderT[F, E, ?], S]

  /*
  gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance Monad m => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance Monad m => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put
    state = Strict.state

instance (Monad m, Monoid w) => MonadState s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put
    state = LazyRWS.state

instance (Monad m, Monoid w) => MonadState s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put
    state = StrictRWS.state

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ExceptT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (IdentityT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ListT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Strict.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state


} */
