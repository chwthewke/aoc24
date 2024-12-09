package aoc24

import cats.effect.Clock
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.either._
import cats.syntax.show._
import scala.concurrent.duration._

object Main extends IOApp {

  val clock: Clock[IO]     = Clock[IO]
  val console: Console[IO] = Console[IO]

  val puzzles: Map[Int, RunnablePuzzle] =
    Vector[RunnablePuzzle]( Aoc1, Aoc2, Aoc3, Aoc4, Aoc5, Aoc6, Aoc7, Aoc8, Aoc9 )
      .map( p => ( p.puzzle.n, p ) )
      .toMap

  def loadInput[G[_]]( puzzle: Puzzle[G], useSample: Boolean, runBonus: Boolean ): IO[Input] =
    (if (useSample) Loader.samples else Loader.inputs).load[IO]( puzzle, runBonus )

  def runPuzzle( n: Int, useSample: Boolean, runBonus: Boolean ): IO[String] =
    for {
      puzzle <- puzzles.get( n ).toRight( new RuntimeException( s"Puzzle #$n not found" ) ).liftTo[IO]
      input  <- loadInput( puzzle.puzzle, useSample, runBonus )
      result <- if (runBonus) puzzle.runBonus( input ) else puzzle.run( input )
    } yield result

  override def run( args: List[String] ): IO[ExitCode] =
    args match {
      case code :: Nil =>
        val ( code1, useSample ) = ( code.stripSuffix( "?" ), code.endsWith( "?" ) )
        val ( code2, runBonus )  = ( code1.stripSuffix( "*" ), code1.endsWith( "*" ) )

        for {
          n      <- Either.catchNonFatal( code2.toInt ).liftTo[IO]
          s      <- clock.monotonic
          result <- runPuzzle( n, useSample, runBonus ).timeout( 1.minute ).attempt
          e      <- clock.monotonic
          _      <- console.errorln( show"[${(e - s).toMillis} ms]" )
          code <- result.fold(
                   err => console.printStackTrace( err ).as( ExitCode.Error ),
                   str => console.println( str ).as( ExitCode.Success )
                 )
        } yield code
      case _ =>
        IO.println( "Invalid arguments" ).as( ExitCode.Error )
    }

}
