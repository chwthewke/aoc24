package aoc24

import cats.effect.Clock
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.either._
import cats.syntax.show._

object Main extends IOApp {

  val clock: Clock[IO]     = Clock[IO]
  val console: Console[IO] = Console[IO]

  val puzzles: Map[Int, RunnablePuzzle] =
    Vector[RunnablePuzzle](
      Aoc1,
      Aoc2,
      Aoc3,
      Aoc4,
      Aoc5,
      Aoc6,
      Aoc7,
      Aoc8,
      Aoc9,
      Aoc10,
      Aoc11,
      Aoc12,
      Aoc13,
      Aoc14,
      Aoc15,
      Aoc16,
      Aoc17,
      Aoc18,
      Aoc19,
      Aoc20,
      Aoc21,
      Aoc22,
      Aoc23,
      Aoc24,
      Aoc25
    ).map( p => ( p.puzzle.puzzleNumber, p ) ).toMap

  def loadInput[G[_]]( puzzle: Puzzle[G], useSample: Boolean, runBonus: Boolean ): IO[Input] =
    ( if (useSample) Loader.samples else Loader.inputs ).load[IO]( puzzle, runBonus )

  def runPuzzle( n: Int, useSample: Boolean, runBonus: Boolean ): IO[String] =
    for {
      t0     <- clock.monotonic
      puzzle <- puzzles.get( n ).toRight( new RuntimeException( s"Puzzle #$n not found" ) ).liftTo[IO]
      input  <- loadInput( puzzle.puzzle, useSample, runBonus )
      t1     <- clock.monotonic
      result <- (
                  if (runBonus) puzzle.runBonus( input, IsSample( useSample ) )
                  else puzzle.run( input, IsSample( useSample ) )
                ).timeout( puzzle.puzzle.timeout )
      t2 <- clock.monotonic
      _ <- console.errorln(
             show"[${( t2 - t0 ).toMillis}ms] [setup=${( t1 - t0 ).toMillis}ms] [run=${( t2 - t1 ).toMillis}ms]"
           )
    } yield result

  case class Args( number: Int, useSample: Boolean, runBonus: Boolean )
  object Args {
    def unapply( s: String ): Option[Args] = {
      val ( t, useSample ) = ( s.stripSuffix( "?" ), s.endsWith( "?" ) )
      val ( u, runBonus )  = ( t.stripSuffix( "*" ), t.endsWith( "*" ) )
      u.toIntOption.map( Args( _, useSample, runBonus ) )
    }
  }

  override def run( args: List[String] ): IO[ExitCode] =
    args match {
      case Args( puzzle ) :: Nil =>
        for {
          result <- runPuzzle( puzzle.number, puzzle.useSample, puzzle.runBonus ).attempt
          code <- result.fold(
                    err => console.printStackTrace( err ).as( ExitCode.Error ),
                    str => console.println( str ).as( ExitCode.Success )
                  )
        } yield code
      case _ =>
        IO.println( "Invalid arguments" ).as( ExitCode.Error )
    }

}
