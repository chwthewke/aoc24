package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.matchers.must.Matchers

class Aoc6Spec extends PuzzleSpec( Aoc6, "41", "6" ) with Matchers {
  import Aoc6._

  "The grid parser" should {
    behave like parseTheSampleLines( parsers.row )
  }

  def createsLoop( pos: Aoc6.Pos ): IO[Boolean] =
    loadInput()
      .flatMap( input => parseGrid( input ).leftMap( new RuntimeException( _ ) ).liftTo[IO] )
      .map( _.createsLoop( pos ) )

  def notCreatingALoop( pos: Aoc6.Pos ): Unit =
    "not create a loop" in {
      createsLoop( pos ).unsafeRunSync() must ===( false )
    }

  def creatingALoop( pos: Aoc6.Pos ): Unit =
    "create a loop" in {
      createsLoop( pos ).unsafeRunSync() must ===( true )
    }

  "Adding an obstruction" which {
    "is at (0, 0)" should {
      behave like notCreatingALoop( Pos( 0, 0 ) )
    }

    "is at (3, 6)" should {
      behave like creatingALoop( Pos( 3, 6 ) )
    }
  }
}
