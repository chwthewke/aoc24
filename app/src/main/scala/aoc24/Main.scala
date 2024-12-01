package aoc24

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    IO.println( s"${buildinfo.Aoc24.name} ${buildinfo.Aoc24.version}" ) *>
      IO.println( s"The answer is ${Library.function}" ).as( ExitCode.Success )

}
