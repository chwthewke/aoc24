package aoc24

class Aoc2Spec extends PuzzleSpec( Aoc2, "2", "4" ) {
  "The report parser" should {
    behave like parseTheSampleLines( Aoc2.reportParser )
  }
}
