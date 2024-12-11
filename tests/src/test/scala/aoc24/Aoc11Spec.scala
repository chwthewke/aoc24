package aoc24

class Aoc11Spec extends PuzzleSpec( Aoc11, "55312", "" ) {
  "The parser" should {
    behave like parseTheTrimmedSample( Aoc11.parsers.stones )
  }
}
