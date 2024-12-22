package aoc24

import cats.Show
import cats.data.NonEmptyVector
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

object Aoc12 extends Puzzle[Either[String, *]]( 12 ) {
  case class Pos( x: Int, y: Int ) {
    def left: Pos               = Pos( x - 1, y )
    def right: Pos              = Pos( x + 1, y )
    def up: Pos                 = Pos( x, y - 1 )
    def down: Pos               = Pos( x, y + 1 )
    def neighbours: Vector[Pos] = Vector( left, up, right, down )

    override def toString: String = s"($x, $y)"
  }

  object Pos {
    implicit val posShow: Show[Pos] = Show.fromToString
  }

  case class Grid( rows: NonEmptyVector[NonEmptyVector[Char]] ) {
    val height: Int = rows.length
    val width: Int  = rows.head.length

    def regions: Map[Char, Vector[Set[Pos]]] =
      rows.iterator.zipWithIndex
        .flatMap { case ( row, y ) => row.iterator.zipWithIndex.map { case ( c, x ) => ( c, Pos( x, y ) ) } }
        .foldLeft( Map.empty[Char, Set[Pos]] ) { case ( acc, ( c, p ) ) => acc |+| Map( c -> Set( p ) ) }
        .fmap( connected[Pos]( _.neighbours ) )

    def connected[P]( neighbours: P => Vector[P] )( ps: Set[P] ): Vector[Set[P]] = {
      @tailrec
      def go( acc: Vector[Set[P]], curr: Set[P], next: Vector[P], open: Set[P] ): Vector[Set[P]] = {
        def pushCurr: Vector[Set[P]] = if (curr.isEmpty) acc else acc :+ curr

        if (open.isEmpty)
          pushCurr
        else if (next.isEmpty) {
          val c1: P      = open.head
          val o1: Set[P] = open.tail
          go( pushCurr, Set( c1 ), Vector( c1 ), o1 )
        } else {
          val p: P          = next.head
          val n1: Vector[P] = next.tail
          val cc: Vector[P] = neighbours( p ).filter( open )
          go( acc, curr ++ cc, n1 ++ cc, open -- cc )
        }
      }

      go( Vector.empty, Set.empty, Vector.empty, ps )
    }

    def perimeter( region: Set[Pos] ): Int =
      region.unorderedFoldMap( p => p.neighbours.count( q => !region( q ) ) )

    val fence: Int = regions.unorderedFoldMap( _.unorderedFoldMap( r => perimeter( r ) * r.size ) )

    // part2

    sealed abstract class Direction( val move: Pos => Pos ) extends EnumEntry
    object Direction extends Enum[Direction] {
      case object Up    extends Direction( _.up )
      case object Right extends Direction( _.right )
      case object Down  extends Direction( _.down )
      case object Left  extends Direction( _.left )

      override def values: Vector[Direction] = findValues.toVector
    }

    type FencePos = ( Pos, Direction )

    def fences( region: Set[Pos] ): Set[FencePos] =
      region.unorderedFoldMap( p =>
        Direction.values.mapFilter( d => Option.when( !region( d.move( p ) ) )( ( p, d ) ) ).toSet
      )

    def fenceNeighbours( fencePos: FencePos ): Vector[FencePos] = fencePos match {
      case ( p, d @ ( Direction.Left | Direction.Right ) ) => Vector( ( p.up, d ), ( p.down, d ) )
      case ( p, d @ ( Direction.Up | Direction.Down ) )    => Vector( ( p.left, d ), ( p.right, d ) )
    }

    def sides( region: Set[Pos] ): Vector[Set[( Pos, Direction )]] =
      connected[FencePos]( fenceNeighbours )( fences( region ) )

    val bulkFence: Int = regions.unorderedFoldMap( _.unorderedFoldMap( r => sides( r ).size * r.size ) )

  }

  def parseGrid( input: Input ): Either[String, Grid] =
    input.lines.toNev
      .toRight( "No rows" )
      .flatMap( _.traverse( _.toVector.toNev.toRight( "Empty row" ) ) )
      .map( Grid )

  override def run( input: Input ): Either[String, String] =
    parseGrid( input )
      .map( _.fence )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseGrid( input )
      .map( _.bulkFence )
      .map( _.toString )
}
