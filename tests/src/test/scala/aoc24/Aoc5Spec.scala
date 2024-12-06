package aoc24

class Aoc5Spec extends PuzzleSpec( Aoc5, "143", "123" ) {
  "The instructions parser" should {
    parseTheRawSample( Aoc5.parsers.instructions )
  }

}
