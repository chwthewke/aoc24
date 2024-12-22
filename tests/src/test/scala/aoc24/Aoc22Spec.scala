package aoc24

import org.scalatest.matchers.must.Matchers

class Aoc22Spec extends PuzzleSpec( Aoc22, "37327623", "23" ) with Matchers {

  import Aoc22._

  "The successive secret numbers generated from 123" should {
    "be as expected" in {

      val it: Iterator[SecretNumber] = new Iterator[SecretNumber] {
        var sn: SecretNumber = SecretNumber( 123 )

        override def hasNext: Boolean = true

        override def next(): SecretNumber = {
          sn = sn.next
          sn
        }
      }

      it.take( 10 ).map( _.value ).toVector must ===(
        Vector(
          15887950L, 16495136L, 527345L, 704524L, 1553684L, 12683156L, 11100544L, 12249484L, 7753432L, 5908254L
        )
      )

    }
  }

  "The successive change sequences and prices generated from 123" should {
    "be as expected" in {
      val expected: Vector[( Int, ( Int, Int, Int, Int ) )] = {
        Vector(
          ( 4, ( -3, 6, -1, -1 ) ),
          ( 4, ( 6, -1, -1, 0 ) ),
          ( 6, ( -1, -1, 0, 2 ) ),
          ( 4, ( -1, 0, 2, -2 ) ),
          ( 4, ( 0, 2, -2, 0 ) ),
          ( 2, ( 2, -2, 0, -2 ) )
        )
      }

      val actual: Vector[( Int, ( Int, Int, Int, Int ) )] =
        SecretNumber( 123 ).pricesAndChanges
          .take( 6 )
          .toVector
          .map( pac => ( pac.price, pac.changes.changes ) )

      actual must ===( expected )

    }
  }

}
