package aoc24

import cats.Eval
import cats.Functor
import cats.Id
import cats.Show
import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all._

sealed trait Runnable[F[_]] {
  def run( fa: => F[String] ): IO[String]
}

object Runnable {
  implicit val runnableId: Runnable[Id] = new Runnable[Id] {
    override def run( fa: => String ): IO[String] = IO.interruptible( fa )
  }

  implicit val runnableEval: Runnable[Eval] = new Runnable[Eval] {
    override def run( fa: => Eval[String] ): IO[String] = IO.interruptible( fa.value )
  }

  implicit val runnableIO: Runnable[IO] = new Runnable[IO] {
    override def run( fa: => IO[String] ): IO[String] = fa
  }

  implicit def runnableEither[E: Show]: Runnable[Either[E, *]] = new Runnable[Either[E, *]] {
    override def run( fa: => Either[E, String] ): IO[String] =
      IO.interruptible( fa.fold( _.show, identity ) )
  }

  implicit def runnableEitherT[F[_]: Functor, E: Show]( implicit R: Runnable[F] ): Runnable[EitherT[F, E, *]] =
    new Runnable[EitherT[F, E, *]] {
      override def run( fa: => EitherT[F, E, String] ): IO[String] =
        R.run( fa.valueOr( _.show ) )
    }

}
