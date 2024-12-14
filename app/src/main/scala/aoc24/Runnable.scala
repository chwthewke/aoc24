package aoc24

import cats.Eval
import cats.Functor
import cats.Id
import cats.Show
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.all._

sealed trait Runnable[F[_]] {
  def run( fa: => F[String], isSample: IsSample ): IO[String]
}

object Runnable {

  implicit val runnableId: Runnable[Id] = new Runnable[Id] {
    override def run( fa: => String, isSample: IsSample ): IO[String] = IO.interruptible( fa )
  }

  implicit val runnableEval: Runnable[Eval] = new Runnable[Eval] {
    override def run( fa: => Eval[String], isSample: IsSample ): IO[String] = IO.interruptible( fa.value )
  }

  implicit val runnableIO: Runnable[IO] = new Runnable[IO] {
    override def run( fa: => IO[String], isSample: IsSample ): IO[String] = fa
  }

  implicit def runnableEither[E: Show]: Runnable[Either[E, *]] = new Runnable[Either[E, *]] {
    override def run( fa: => Either[E, String], isSample: IsSample ): IO[String] =
      IO.interruptible( fa.fold( _.show, identity ) )
  }

  implicit def runnableEitherT[F[_]: Functor, E: Show]( implicit R: Runnable[F] ): Runnable[EitherT[F, E, *]] =
    new Runnable[EitherT[F, E, *]] {
      override def run( fa: => EitherT[F, E, String], isSample: IsSample ): IO[String] =
        R.run( fa.valueOr( _.show ), isSample )
    }

  implicit def runnableKleisli[F[_]]( implicit R: Runnable[F] ): Runnable[Kleisli[F, IsSample, *]] =
    new Runnable[Kleisli[F, IsSample, *]] {
      override def run( fa: => Kleisli[F, IsSample, String], isSample: IsSample ): IO[String] =
        R.run( fa.run( isSample ), isSample )
    }
}
