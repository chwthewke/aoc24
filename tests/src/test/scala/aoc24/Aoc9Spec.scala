package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers

class Aoc9Spec extends PuzzleSpec( Aoc9, "1928", "2858" ) with Matchers {
  import Aoc9._

  "The layout parser" should {
    parseTheTrimmedSampleAs( parsers.layout ) {
      case Layout( _, files, _ ) =>
        files must ===(
          Vector(
            ( 0, File( 0, 2 ) ),
            ( 5, File( 1, 3 ) ),
            ( 11, File( 2, 1 ) ),
            ( 15, File( 3, 3 ) ),
            ( 19, File( 4, 2 ) ),
            ( 22, File( 5, 4 ) ),
            ( 27, File( 6, 4 ) ),
            ( 32, File( 7, 3 ) ),
            ( 36, File( 8, 4 ) ),
            ( 40, File( 9, 2 ) )
          )
        )

    }
  }

  def loadLayout( load: IO[Input] = loadSampleInput() ): IO[Layout] =
    load
      .flatMap(
        input =>
          parsers.layout
            .parseAll( input.trimmed )
            .leftMap( err => new RuntimeException( s"Parser error $err" ) )
            .liftTo[IO]
      )

  "The layout random access" should {
    def returningFileAt( expFileId: Option[Int], ix: Int ): Unit =
      s"return file ${expFileId.fold( "-" )( n => s"#$n" )} at $ix" in {
        val layout: Layout = loadLayout().unsafeRunSync()

        val actual: Option[File] = layout.at( ix )

        actual.map( _.id ) must ===( expFileId )
      }

    // 000000000011111111112222222222233333333344
    // 012345678901234567890123456789012345678901
    //
    // 00...111...2...333.44.5555.6666.777.888899

    behave like returningFileAt( Some( 0 ), 0 )
    behave like returningFileAt( Some( 0 ), 1 )
    behave like returningFileAt( None, 4 )
    behave like returningFileAt( Some( 1 ), 5 )
    behave like returningFileAt( Some( 1 ), 7 )
    behave like returningFileAt( Some( 9 ), 41 )
  }

  "Reading the free spaces" should {
    "produce the expected sequence" in {
      val layout: Layout                   = loadLayout().unsafeRunSync()
      val freeSpaces: Vector[( Int, Int )] = layout.getFreeSpaces.toVector

      val expected: Vector[( Int, Int )] =
        Vector(
          ( 2, 3 ),
          ( 8, 3 ),
          ( 12, 3 ),
          ( 18, 1 ),
          ( 21, 1 ),
          ( 26, 1 ),
          ( 31, 1 ),
          ( 35, 1 )
        )

      freeSpaces must ===( expected )
    }
  }

  "The compacted (whole files) layout" should {
    "be as expected" in {
      val layout: Layout    = loadLayout().unsafeRunSync()
      val compacted: Layout = layout.compactWholeFiles

      compacted.toStringSimple must ===( "00992111777.44.333....5555.6666.....8888.." )
    }

    "have no overlap" when {
      def testNoOverlap( load: IO[Layout] ): Assertion = {
        val layout    = load.unsafeRunSync()
        val compacted = layout.compactWholeFiles

        val overlaps: List[( ( Int, File ), ( Int, File ) )] =
          compacted.filesByPosition.toVector.sliding2
            .filter { case ( ( p1, f1 ), ( p2, _ ) ) => p2 < p1 + f1.size }

        overlaps mustBe empty
      }

      "run against the sample input" in {
        testNoOverlap( loadLayout( loadSampleInput() ) )
      }

      "run against the real input" ignore {
        testNoOverlap( loadLayout( loadRealInput() ) )
      }
    }
  }

}
