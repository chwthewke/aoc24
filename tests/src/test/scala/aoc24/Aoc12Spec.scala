package aoc24

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.matchers.must.Matchers

class Aoc12Spec extends PuzzleSpec( Aoc12, "1930", "1206" ) with Matchers {

  import Aoc12._

  "The sample input" should {
    "have 11 regions" in {
      loadSampleInput()
        .flatMap( parseGrid( _ ).leftMap( new RuntimeException( _ ) ).liftTo[IO] )
        .fproduct( _.regions )
        .flatTap {
          case ( grid, regs ) =>
            IO.println(
              regs
                .map {
                  case ( c, crs ) =>
                    val allP = crs.combineAll
                    val repr =
                      (0 until grid.height)
                        .map( y => (0 until grid.width).map( x => if (allP( Pos( x, y ) )) c else '.' ).mkString )
                        .mkString( "\n" )

                    val ss = crs.map( _.size )

                    s"$c: ${crs.size} Regions (${ss.mkString( ", " )})\n$repr"
                }
                .mkString( "\n\n" )
            )
        }
        .map( _._2 )
        .map( _.unorderedFoldMap( _.size ) )
        .unsafeRunSync() must ===( 11 )
    }
  }

}
