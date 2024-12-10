package aoc24

class Aoc10Spec extends PuzzleSpec( Aoc10, "36", "81" ) {
  "The row parser" should {
    behave like parseTheSampleLines( Aoc10.parsers.row )
  }

}
