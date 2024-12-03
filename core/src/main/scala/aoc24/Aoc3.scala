package aoc24

import cats.data.NonEmptyList
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all._

object Aoc3 extends Puzzle[Either[String, *]]( 3 ) {

  val mulArg: Parser[Int] = Numbers.digit.rep( 1, 3 ).string.mapFilter( _.toIntOption )

  val mulOp: Parser[Int] =
    ( Parser.string( "mul(" ), mulArg, Parser.char( ',' ), mulArg, Parser.char( ')' ) )
      .mapN( ( _, m, _, n, _ ) => m * n )

  val doOp: Parser[Unit]   = Parser.string( "do()" )
  val dontOp: Parser[Unit] = Parser.string( "don't()" )

  val parser: Parser[NonEmptyList[Int]] = mulOp.backtrack.orElse( Parser.anyChar.as( 0 ) ).rep

  override def run( input: Input ): Either[String, String] =
    parser
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )
      .map( _.toList.sum.toString )

  val bonusParser: Parser[NonEmptyList[Either[Boolean, Int]]] =
    (mulOp.map( Right( _ ) ) | dontOp.as( Left( false ) ) | doOp.as( Left( true ) )).backtrack
      .orElse( Parser.anyChar.as( 0 ).map( Right( _ ) ) )
      .rep

  override def runBonus( input: Input ): Either[String, String] =
    bonusParser
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )
      .map(
        list =>
          list
            .foldLeft( ( 0, true ) ) {
              case ( ( acc, flag ), Right( n ) ) if flag => ( acc + n, flag )
              case ( ( acc, _ ), Left( f ) )             => ( acc, f )
              case ( z, _ )                              => z
            }
            ._1
            .toString
      )
}
