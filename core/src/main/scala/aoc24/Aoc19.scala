package aoc24

import cats.effect.IO
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Aoc19 extends Puzzle[IO]( 19 ) {

  case class RadixTree( matches: Boolean, byChar: Map[Char, RadixTree] ) {
    def add( s: String ): RadixTree =
      s.headOption match {
        case None => copy( matches = true )
        case Some( c ) =>
          copy( byChar = byChar.updatedWith( c )( _.getOrElse( RadixTree.empty ).add( s.drop( 1 ) ).some ) )
      }

    def prefixesOf( s: String ): Set[Int] =
      (s.headOption match {
        case None      => Set.empty[Int]
        case Some( c ) => byChar.get( c ).foldMap( t => t.prefixesOf( s.drop( 1 ) ).map( _ + 1 ) )
      }) ++ Option.when( matches )( 0 )

    def canMake( s: String ): Boolean = {
      @tailrec
      def go( seen: Set[Int], open: Vector[Int] ): Boolean =
        open.headOption match {
          case None => false
          case Some( p ) =>
            if (p == s.length) true
            else if (seen( p ))
              go( seen, open.drop( 1 ) )
            else {
              val next: Set[Int]       = prefixesOf( s.drop( p ) ).map( _ + p )
              val newOpen: Vector[Int] = (next.toVector.sorted.reverse ++ open.drop( 1 )).distinct
              go( seen.incl( p ), newOpen )
            }
        }

      go( Set.empty, Vector( 0 ) )
    }

    def countWaysToMake( s: String ): Long = {
      def finish( backtrack: SortedMap[Int, Set[Int]] ): Long = {
        if (backtrack.contains( s.length ))
          backtrack.toVector
            .foldLeft( SortedMap.empty[Int, Long] ) {
              case ( acc, ( p, c ) ) =>
                acc.updated( p, if (p == 0) 1L else c.unorderedFoldMap( i => acc.getOrElse( i, 0 ) ) )
            }
            .getOrElse( s.length, 0L )
        else 0L
      }

      @tailrec
      def go( seen: SortedMap[Int, Set[Int]], open: Vector[( Int, Option[Int] )] ): Long =
        open.headOption match {
          case None =>
            finish( seen )
          case Some( ( p, prev ) ) =>
            seen.get( p ) match {
              case None =>
                val next = prefixesOf( s.drop( p ) ).map( _ + p ).toVector
                go( seen.updated( p, prev.toSet ), open.tail ++ next.tupleRight( p.some ) )
              case Some( v ) =>
                go( seen.updated( p, v ++ prev ), open.tail )
            }
        }

      go( SortedMap.empty, Vector( ( 0, None ) ) )
    }
  }

  object RadixTree {
    val empty: RadixTree = RadixTree( matches = false, Map.empty )
    def apply( strings: Vector[String] ): RadixTree =
      strings.foldLeft( empty )( _.add( _ ) )
  }

  case class Designs( towels: RadixTree, designs: Vector[String] ) {

    def countValid: IO[Int] =
      designs
        .grouped( designs.size / 8 )
        .toVector
        .parFoldMapA( batch => IO.blocking( batch.count( towels.canMake ) ) )

    def countWaysToMakeAll: IO[Long] =
      designs
        .grouped( designs.size / 8 )
        .toVector
        .parFoldMapA( batch => IO.blocking( batch.foldMap( towels.countWaysToMake ) ) )

  }

  object parsers {
    def endl: Parser[Unit] = Rfc5234.lf | (Rfc5234.cr ~ Rfc5234.lf.rep0).void

    val stripe: Parser[Char]  = Parser.charIn( "wubrg" )
    val towel: Parser[String] = stripe.rep.string
    val towels: Parser[Vector[String]] =
      towel.repSep( Parser.char( ',' ) *> Rfc5234.wsp ).map( _.toNev.toVector )

    val designs: Parser[Designs] =
      ( towels <* endl <* endl, towel.repSep( endl ) <* endl.rep0 )
        .mapN( ( towels, designs ) => Designs( RadixTree( towels ), designs.toNev.toVector ) )
  }

  def parseDesigns( input: Input ): IO[Designs] =
    parsers.designs
      .parseAll( input.raw )
      .leftMap( err => new RuntimeException( s"Parser error $err" ) )
      .liftTo[IO]

  override def run( input: Input ): IO[String] =
    parseDesigns( input )
      .flatMap( _.countValid )
      .map( _.toString )

  override def runBonus( input: Input ): IO[String] =
    parseDesigns( input )
      .flatMap( _.countWaysToMakeAll )
      .map( _.toString )
}
