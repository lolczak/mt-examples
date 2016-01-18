package org.lolczak.mt

import org.lolczak.lambda._

import scala.App
import scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._
import org.lolczak.lambda.{App => Apply,_}
import scalaz.Scalaz._

object EitherRwstExample extends App {

  type StateType = Int

  type Logs = List[String]
  type Eval[A] = EitherT[RWST[IO, Env, Logs, StateType, ?], Failure, A]

  def runEval[A](env: Env, st: StateType, op: Eval[A]): IO[(Logs, Failure \/ A, StateType)] = {
    op.run.run(env, st)
  }

  def eval(exp: Exp): Eval[LambdaValue] = {
    import MtInstances._
    implicit val E4 = implicitly[MonadTrans[λ[(α[_], β) => RWST[α, Env, Logs, StateType, β]]]]
    implicit val E0 = implicitly[MonadTrans[λ[(α[_], β) => EitherT[α, Failure, β]]]]
    import E0._
    implicit val E1 = MtInstances.eitherTMonadReader[RWST[IO, Env, Logs, StateType, ?], Env, Failure] //implicitly[MonadReader[Eval, Env]]
    import E1._
    implicit val E2 = EitherT.monadListen[RWST[IO, Env, Logs, StateType, ?], Logs, Failure] //implicitly[MonadListen[Eval, Logs]]
    import E2._
    implicit val E3 = EitherT.eitherTMonadError[RWST[IO, Env, Logs, StateType, ?], Failure]//implicitly[MonadError[Eval, Failure]]
    implicit val MS = eitherTMonadState[RWST[IO, Env, Logs, StateType, ?], StateType, Failure]
    import E3._
    implicit val E5 = EitherT.eitherTMonad[RWST[IO, Env, Logs, StateType, ?], Failure]//implicitly[Monad[Eval]]

    exp match {
      case Lit(i) =>
        for {
          _ <- tick[Eval]
          _ <- E0.liftMU(E4.liftM(putStrLn(s"Lit $i")))
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
          result <- (val1, val2) match {
            case (IntVal(v1), IntVal(v2)) => E5.point(IntVal(v1 + v2).asInstanceOf[LambdaValue])
            case _                        => raiseError(Failure("Type error in addition"))
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
            case _                        => raiseError(Failure("Type error in application"))
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


  val exp = Add(Lit(12), Apply(Abs("x", Var("x")), Add(Lit(4), Lit(2))))

  val result = runEval(Map.empty, 0, eval(exp))

  println(result.unsafePerformIO())

}

object MtInstances {

  implicit def eitherTMonadReader[F[_], En, Er](implicit MR0: MonadReader[F, Env]) = new MonadReader[EitherT[F, Er, ?], En] {

    override def ask: EitherT[F, Er, En] = ???

    override def local[A](f: (En) => En)(fa: EitherT[F, Er, A]): EitherT[F, Er, A] = ???

    override def bind[A, B](fa: EitherT[F, Er, A])(f: (A) => EitherT[F, Er, B]): EitherT[F, Er, B] = ???

    override def point[A](a: => A): EitherT[F, Er, A] = ???
  }

  implicit def eitherTMonadState[F[_], S, Er](implicit MS0: MonadState[F, S]): MonadState[EitherT[F, Er, ?], S] = new MonadState[EitherT[F, Er, ?], S] {
    override def init: EitherT[F, Er, S] = ???

    override def get: EitherT[F, Er, S] = ???

    override def put(s: S): EitherT[F, Er, Unit] = ???

    override def bind[A, B](fa: EitherT[F, Er, A])(f: (A) => EitherT[F, Er, B]): EitherT[F, Er, B] = ???

    override def point[A](a: => A): EitherT[F, Er, A] = ???
  }

}