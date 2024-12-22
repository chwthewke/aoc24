package aoc24

import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._

object Aoc1 extends Puzzle[Either[String, *]]( 1 ) {

  private val posInt: Parser[Int] = Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
  val lineParser: Parser[( Int, Int )] =
    ( posInt, Rfc5234.wsp.rep.void, posInt ).mapN( ( m, _, n ) => ( m, n ) )

  case class Data( left: Vector[Int], right: Vector[Int] )

  def parseData( input: Input ): Either[String, Data] =
    input.lines
      .traverse( lineParser.parseAll )
      .leftMap( err => s"Parse error $err" )
      .map { lines =>
        val ( list1, list2 ) = lines.unzip
        Data( list1, list2 )
      }

  def sumOfDistances( data: Data ): Option[Int] = {
    data.left.sorted
      .align( data.right.sorted )
      .foldMapM( _.onlyBoth.map { case ( x, y ) => ( x - y ).abs } )
  }

  def similarity( data: Data ): Int = {
    val occurrences: Map[Int, Int] = data.right.foldMap( n => Map( n -> 1 ) )
    data.left.foldMap( n => occurrences.getOrElse( n, 0 ) * n )
  }

  override def run( input: Input ): Either[String, String] =
    for {
      data   <- parseData( input )
      result <- sumOfDistances( data ).toRight( "Misaligned lists" )
    } yield result.toString

  override def runBonus( input: Input ): Either[String, String] =
    parseData( input ).map( similarity ).map( _.toString )
}
