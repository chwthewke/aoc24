package aoc24

import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers

class Aoc15Spec extends PuzzleSpec( Aoc15, "10092", "9021" ) with Matchers {
  import Aoc15._

  "The grid-and-moves parser" should {
    behave like parseTheRawSampleAs( parsers.gridAndInstructions ) {
      case ( Some( grid ), moves ) =>
        moves.length must ===( 700 )
        grid.width must ===( 10 )
        grid.height must ===( 10 )
        grid.walls.size must ===( 37 )
        grid.crates.size must ===( 21 )
        grid.robot must ===( V2( 4, 4 ) )
    }
  }

  def parseWideGridAndInstructions( src: String ): Either[String, ( WideGrid, List[Direction] )] =
    parseGridAndInstructions( Input( src.linesIterator.toVector ) )
      .map { case ( narrow, instrs ) => ( WideGrid.of( narrow ), instrs ) }

  def testMove( src: String )( assertion: PartialFunction[Grid, Assertion] ): Assertion =
    inside( parseWideGridAndInstructions( src ) ) {
      case Right( ( grid, instrs ) ) => inside( grid.moveAll( instrs ) )( assertion )
    }

  "Moving left on a wide grid" should {
    "push crates" in {
      testMove(
        """#.OO@
          |
          |<
          |""".stripMargin
      ) { grid =>
        grid.crates must ===( Set( V2( 3, 0 ), V2( 5, 0 ) ) )
        grid.robot must ===( V2( 7, 0 ) )
      }
    }

    "move in absence of crates" in {
      testMove(
        """..@
          |
          |<
          |""".stripMargin
      ) { grid =>
        grid.robot must ===( V2( 3, 0 ) )
      }
    }

    "not move against a wall" in {
      testMove(
        """#O@
          |
          |<
          |""".stripMargin
      ) { grid =>
        grid.robot must ===( V2( 4, 0 ) )
      }
    }
  }

  "Moving right on a wide grid" should {
    "push crates" in {
      testMove(
        """@OO.#
          |
          |>>
          |""".stripMargin
      ) { grid =>
        grid.crates must ===( Set( V2( 3, 0 ), V2( 5, 0 ) ) )
        grid.robot must ===( V2( 2, 0 ) )
      }
    }

    "move in absence of crates" in {
      testMove(
        """@..
          |
          |>
          |""".stripMargin
      ) { grid =>
        grid.robot must ===( V2( 1, 0 ) )
      }
    }

    "not move against a wall" in {
      testMove(
        """@O#
          |
          |>>
          |""".stripMargin
      ) { grid =>
        grid.robot must ===( V2( 1, 0 ) )
      }
    }
  }

  "Moving up on a wide grid" should {
    "push crates" in {
      testMove(
        """.....
          |.OOO.
          |..OO@
          |..O..
          |.....
          |
          |<vv<<<^
          |""".stripMargin
      ) { grid =>
        grid.crates must ===(
          Set(
            V2( 2, 0 ),
            V2( 4, 0 ),
            V2( 6, 0 ),
            V2( 3, 1 ),
            V2( 5, 1 ),
            V2( 4, 2 )
          )
        )

      }
    }

    "not move if a single crate hits a wall" in {
      testMove(
        """.#...
          |.OOO.
          |..OO@
          |..O..
          |.....
          |
          |<vv<<<^
          |""".stripMargin
      ) { grid =>
        grid.crates must ===(
          Set(
            V2( 2, 1 ),
            V2( 4, 1 ),
            V2( 6, 1 ),
            V2( 3, 2 ),
            V2( 5, 2 ),
            V2( 4, 3 )
          )
        )
      }

    }
  }

}
