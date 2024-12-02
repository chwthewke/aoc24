package aoc24

class Aoc1Spec extends PuzzleSpec( Aoc1, "11", "31" ) {
  "The line parser" should {
    parseTheSampleLines( Aoc1.lineParser )
  }
}
