package aoc24

import aoc24.Aoc19.Designs
import aoc24.Aoc19.RadixTree
import org.scalatest.matchers.must.Matchers

class Aoc19Spec extends PuzzleSpec( Aoc19, "6", "16" ) with Matchers {
  "The designs parser" should {
    behave like parseTheRawSampleAs( Aoc19.parsers.designs ) {
      case Designs( towels, designs ) =>
        towels must ===(
          RadixTree( Vector( "r", "wr", "b", "g", "bwu", "rb", "gb", "br" ) )
        )

        designs must contain theSameElementsAs Vector(
          "brwrr",
          "bggr",
          "gbbr",
          "rrbgbr",
          "ubwu",
          "bwurrg",
          "brgr",
          "bbrgwb"
        )

    }
  }

}
