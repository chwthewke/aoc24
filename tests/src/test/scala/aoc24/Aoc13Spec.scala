package aoc24

class Aoc13Spec extends PuzzleSpec( Aoc13, "480", "" ) {
  import Aoc13._

  "The claw machine parser" should {
    behave like parseTheTrimmedSampleAs( parsers.machines ) {
      case Vector(
          ClawMachine( V2( 94, 34 ), V2( 22, 67 ), V2( 8400, 5400 ) ),
          ClawMachine( V2( 26, 66 ), V2( 67, 21 ), V2( 12748, 12176 ) ),
          ClawMachine( V2( 17, 86 ), V2( 84, 37 ), V2( 7870, 6450 ) ),
          ClawMachine( V2( 69, 23 ), V2( 27, 71 ), V2( 18641, 10279 ) )
          ) =>
        succeed
    }
  }

}
