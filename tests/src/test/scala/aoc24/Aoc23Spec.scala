package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

class Aoc23Spec extends PuzzleSpec( Aoc23, "7", "co,de,ka,ta" ) {

  import Aoc23._

  "Parsing the input" should {
    "produce the expected network" in {
      val network: Network =
        loadSampleInput().flatMap( parseNetwork( _ ).leftMap( new RuntimeException( _ ) ).liftTo[IO] ).unsafeRunSync()

      println( network )
      succeed
    }
  }

}
