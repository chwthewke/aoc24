package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global

class Aoc14Spec extends PuzzleSpec( Aoc14, "12", "" ) {
  import Aoc14._

  "The robot parser" should {
    behave like parseTheSampleLines( Aoc14.parsers.robot )
  }

  "The sample room after 100 seconds" should {
    def parseSample: IO[SecurityRoom] =
      loadSampleInput().flatMap( parseRobots( _, IsSample( true ) ) )

//    "contain the expected robots" in {
//      val robots =
//        parseSample.map( room => room.robots.map( _.posAt( 100, room.dim ) ) ).unsafeRunSync()
//
//      robots.foreach( println( _ ) )
//
//      succeed
//    }

    "look like this" in {
      parseSample
        .flatMap( secRoom => IO.println( secRoom.show( 100 ) ) )
        .unsafeRunSync()

      succeed
    }
  }
}
