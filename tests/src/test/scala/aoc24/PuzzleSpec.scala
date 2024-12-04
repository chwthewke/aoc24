package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.parse.Parser
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.show._
import org.scalactic.source.Position
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class PuzzleSpec( puzzle: RunnablePuzzle, expectedBasic: String, expectedBonus: String )
    extends AnyWordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with Inside {

  def run( puzzle: RunnablePuzzle, input: Input ): IO[String]      = puzzle.run( input )
  def runBonus( puzzle: RunnablePuzzle, input: Input ): IO[String] = puzzle.runBonus( input )

  def runPuzzle( bonus: Boolean ): IO[String] =
    loadInput( bonus )
      .flatMap( in => if (bonus) runBonus( puzzle, in ) else run( puzzle, in ) )

  private def loadInput( runBonus: Boolean ): IO[Input] =
    Loader.samples.load[IO]( puzzle.puzzle, runBonus )

  private def decl( name: String, expected: String )( test: => Any )( implicit pos: Position ): Unit =
    if (expected == "") name ignore test else name in test

  "The basic problem" should {
    decl( "produce the expected result", expectedBasic ) {
      runPuzzle( bonus = false ).unsafeRunSync() must ===( expectedBasic )
    }
  }

  "The bonus problem" should {
    decl( "produce the expected result", expectedBonus ) {
      runPuzzle( bonus = true ).unsafeRunSync() must ===( expectedBonus )
    }
  }

  private def parseTheSample[A, B](
      parse: ( Input, Parser[A] ) => Either[Parser.Error, B],
      parser: Parser[A],
      bonus: Boolean
  ): Unit =
    s"successfully parse the sample${Option.when( bonus )( " (bonus)" ).orEmpty}" in {
      inside( loadInput( bonus ).map( parse( _, parser ) ).attempt.unsafeRunSync() ) {
        case Left( _ )          => fail( "loadInput failed" )
        case Right( Left( e ) ) => fail( show"parser failed: $e" )
        case _                  => succeed
      }
    }

  def parseTheSampleLines[A]( parser: Parser[A], bonus: Boolean = false ): Unit =
    parseTheSample[A, Vector[A]]( ( input, parser ) => input.lines.traverse( parser.parseAll ), parser, bonus )

  def parseTheTrimmedSample[A]( parser: Parser[A], bonus: Boolean = false ): Unit =
    parseTheSample[A, A]( ( input, parser ) => parser.parseAll( input.trimmed ), parser, bonus )

  def parseTheRawSample[A]( parser: Parser[A], bonus: Boolean = false ): Unit =
    parseTheSample[A, A]( ( input, parser ) => parser.parseAll( input.raw ), parser, bonus )

}
