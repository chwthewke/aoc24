package aoc24

import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.data.State
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._

object Aoc11 extends Puzzle[Either[String, *]]( 11 ) {
  private def bigIntOf( s: String ): Option[BigInt] =
    Either.catchNonFatal( BigInt( s ) ).toOption

  object parsers {
    val stones: Parser[NonEmptyVector[BigInt]] =
      Numbers.nonNegativeIntString
        .mapFilter( bigIntOf )
        .repSep( Rfc5234.wsp.rep )
        .map( _.toNev )
  }

  def blinkOnce( stone: BigInt ): Vector[BigInt] =
    if (stone == 0)
      Vector( BigInt( 1 ) )
    else {
      val str: String = stone.toString()
      if (str.length % 2 == 0) {
        val half: Int = str.length / 2
        Vector( str.take( half ), str.drop( half ) ).mapFilter( bigIntOf )
      } else
        Vector( stone * 2024 )
    }

  type Memo = Map[( BigInt, Int ), Long]

  def blinkMany( stone: BigInt, n: Int ): State[Memo, Long] =
    if (n == 0) State.pure( 1L )
    else
      OptionT( State.inspect[Memo, Option[Long]]( _.get( ( stone, n ) ) ) )
        .getOrElseF(
          blinkOnce( stone )
            .foldMapM( st => blinkMany( st, n - 1 ) )
            .flatTap( c => State.modify[Memo]( _.updated( ( stone, n ), c ) ) )
        )

  def runSync( stones: NonEmptyVector[BigInt], n: Int ): Long =
    stones.foldMapM( blinkMany( _, n ) ).runA( Map.empty ).value

  private def parseStones( input: Input ): Either[String, NonEmptyVector[BigInt]] =
    parsers.stones
      .parseAll( input.trimmed )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    parseStones( input )
      .map( runSync( _, 25 ) )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseStones( input )
      .map( runSync( _, 75 ) )
      .map( _.toString )

}
