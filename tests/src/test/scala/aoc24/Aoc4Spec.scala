package aoc24

import org.scalatest.matchers.must.Matchers
import aoc24.Aoc4.Haystack

class Aoc4Spec extends PuzzleSpec( Aoc4, "18", "9" ) with Matchers {

  "sliding diag" should {
    "produce the sliding diagonal windows" in {
      val haystack =
        Haystack(
          Input(
            Vector(
              "abc",
              "def",
              "ghi"
            )
          )
        )

      val expected = Vector( "ae", "bf", "dh", "ei" ).map( _.toVector )

      haystack.slidingDiag( 2 ).toVector must contain theSameElementsAs expected

    }
  }

}
