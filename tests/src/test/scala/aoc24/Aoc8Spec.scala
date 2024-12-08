package aoc24

class Aoc8Spec extends PuzzleSpec( Aoc8, "14", "34" ) {
  "The row parser" should {
    behave like parseTheSampleLines( Aoc8.parsers.row )
  }

}
