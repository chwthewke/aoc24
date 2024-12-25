package aoc24

import cats.data.EitherT
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._

object Aoc25 extends Puzzle[Either[String, *]]( 25 ) {

  case class Lock( heights: Vector[Int] )
  case class Key( heights: Vector[Int] )

  case class OfficeFloor(
      itemHeight: Int,
      locks: Vector[Lock],
      keys: Vector[Key]
  ) {
    def possibleMatch( key: Key, lock: Lock ): Boolean =
      key.heights.alignCombine( lock.heights ).forall( _ <= itemHeight )

    def countPossibleKeyLockCombinations: Int =
      keys.foldMap( k => locks.count( l => possibleMatch( k, l ) ) )

  }

  object OfficeFloor

  object parsers {

    def parseKeyOrLock( lines: Vector[String] ): Either[String, Either[Key, Lock]] = {
      val `type`: Either[String, Vector[Int] => Either[Key, Lock]] =
        lines.headOption match {
          case Some( v ) if v.forall( _ == '.' ) => Right( ( h: Vector[Int] ) => Left( Key( h ) ) )
          case Some( v ) if v.forall( _ == '#' ) => Right( ( h: Vector[Int] ) => Right( Lock( h ) ) )
          case _                                 => Left( lines.mkString( "Neither key nor lock:\n", "\n", "" ) )
        }

      `type`.map( _.apply( lines.transpose.map( _.count( _ == '#' ) - 1 ) ) )
    }

    def endl: Parser[Unit] = Rfc5234.lf | ( Rfc5234.cr ~ Rfc5234.lf.rep0 ).void

    val line: Parser[String] = Parser.charIn( ".#" ).rep.string

    val rawKeyOrLock: Parser[Vector[String]] = line.repSep( endl ).map( _.toNev.toVector )

    val rawKeysOrLocks: Parser[Vector[Vector[String]]] =
      rawKeyOrLock.repSep( endl.rep ).map( _.toNev.toVector ) <* endl.rep

    def parseOfficeFloor( input: Input ): Either[String, OfficeFloor] =
      rawKeysOrLocks
        .parseAll( input.raw )
        .leftMap( err => s"Parser error $err" )
        .flatMap { rawItems =>
          val ( heights, items ) =
            rawItems.traverse( lines => EitherT( ( Set( lines.size ), parseKeyOrLock( lines ) ) ) ).value

          items.flatMap { keysAndLocks =>
            val ( keys, locks ) = keysAndLocks.partitionEither( identity )
            Option
              .when( heights.size == 1 )( OfficeFloor( heights.head - 2, locks, keys ) )
              .toRight( "multiple heights" )
          }
        }
  }

  override def run( input: Input ): Either[String, String] =
    parsers
      .parseOfficeFloor( input )
      .map( _.countPossibleKeyLockCombinations )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] = ???
}
