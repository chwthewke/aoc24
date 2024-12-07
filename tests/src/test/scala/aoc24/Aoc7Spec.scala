package aoc24

class Aoc7Spec extends PuzzleSpec( Aoc7, "3749", "11387" ) {
  "The equation parser" should {
    behave like parseTheSampleLines( Aoc7.parsers.equation )
  }

}
