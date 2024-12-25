package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.matchers.must.Matchers

class Aoc25Spec extends PuzzleSpec( Aoc25, "3", "" ) with Matchers {
  "The parse function" should {
    "succeed" in {
      inside(
        loadSampleInput()
          .flatMap( Aoc25.parsers.parseOfficeFloor( _ ).leftMap( new RuntimeException( _ ) ).liftTo[IO] )
          .attempt
          .unsafeRunSync()
      ) {
        case Right( of ) =>
          of.itemHeight must ===( 5 )
          ( of.keys.size + of.locks.size ) must ===( 5 )
      }
    }
  }

}
