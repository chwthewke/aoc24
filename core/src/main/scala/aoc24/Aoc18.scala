package aoc24

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.Kleisli
import cats.derived.semiauto
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

object Aoc18 extends Puzzle[Kleisli[Either[String, *], IsSample, *]]( 18 ) {

  case class V2( x: Int, y: Int ) {
    override def toString: String = s"($x, $y)"
  }

  object V2 {
    implicit val v2Monoid: Monoid[V2]     = semiauto.monoid[V2]
    val zero: V2                          = v2Monoid.empty
    implicit val v2Show: Show[V2]         = Show.fromToString
    implicit val v2Order: Order[V2]       = semiauto.order[V2]
    implicit val v2Ordering: Ordering[V2] = Order.catsKernelOrderingForOrder
  }

  sealed abstract class Direction( val v: V2, val repr: Char ) extends EnumEntry

  object Direction extends Enum[Direction] {
    case object Up    extends Direction( V2( 0, -1 ), '^' )
    case object Right extends Direction( V2( 1, 0 ), '>' )
    case object Down  extends Direction( V2( 0, 1 ), 'v' )
    case object Left  extends Direction( V2( -1, 0 ), '<' )

    override val values: Vector[Direction]              = findValues.toVector
    implicit val directionOrder: Order[Direction]       = Order.by( _.v )
    implicit val directionOrdering: Ordering[Direction] = Order.catsKernelOrderingForOrder
  }

  def gridDim( isSample: IsSample ): V2 =
    if (isSample.value) V2( 7, 7 ) else V2( 71, 71 )

  def oneKilobyte( isSample: IsSample ): Int =
    if (isSample.value) 12
    else 1024

  case class Grid( dim: V2, obstacles: Set[V2] ) {
    def neighbours( p: V2 ): Vector[V2] =
      Direction.values.mapFilter( d =>
        ( p |+| d.v ).some
          .filter( q =>
            q.x >= 0 && q.x < dim.x &&
              q.y >= 0 && q.y < dim.y &&
              !obstacles( q )
          )
      )

    val start: V2 = V2( 0, 0 )
    val goal: V2  = dim |+| V2( -1, -1 )

    def path: Option[Int] = {
      @tailrec
      def go( seen: Map[V2, Int], next: Vector[( Int, V2 )] ): Option[Int] =
        next.headOption match {
          case None => None
          case Some( ( c, p ) ) =>
            if (p == goal)
              Some( c )
            else if (seen.get( p ).exists( _ <= c ))
              go( seen, next.drop( 1 ) )
            else go( seen.updated( p, c ), next ++ neighbours( p ).tupleLeft( c + 1 ) )
        }

      go( Map.empty, Vector( ( 0, start ) ) )
    }
  }

  object parsers {
    val int: Parser[Int] = Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
    val pos: Parser[V2]  = ( int, Parser.char( ',' ), int ).mapN( ( x, _, y ) => V2( x, y ) )
  }

  def parseBytes( input: Input, limit: Option[Int] ): Either[String, Vector[V2]] =
    limit
      .foldLeft( input.lines )( _.take( _ ) )
      .traverse( parsers.pos.parseAll )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Kleisli[Either[String, *], IsSample, String] =
    Kleisli( isSample =>
      parseBytes( input, Some( oneKilobyte( isSample ) ) )
        .map( bytes => Grid( gridDim( isSample ), bytes.toSet ) )
        .flatMap( _.path.toRight( "No path found" ) )
        .map( _.toString )
    )

  def findSplittingByte( bytes: Vector[V2], isSample: IsSample ): V2 = {
    @tailrec
    def bisect( good: Int, bad: Int ): V2 =
      if (bad - good == 1)
        bytes( bad - 1 )
      else {
        val p: Int = ( bad + good ) / 2
        if (Grid( gridDim( isSample ), bytes.take( p ).toSet ).path.isDefined)
          bisect( p, bad )
        else
          bisect( good, p )
      }

    bisect( oneKilobyte( isSample ), bytes.size )
  }

  override def runBonus( input: Input ): Kleisli[Either[String, *], IsSample, String] =
    Kleisli( isSample =>
      parseBytes( input, None )
        .map( findSplittingByte( _, isSample ) )
        .map( p => s"${p.x},${p.y}" )
    )
}
