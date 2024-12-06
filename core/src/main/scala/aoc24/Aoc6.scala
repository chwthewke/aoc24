package aoc24

import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.effect.IO
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import fs2.Stream
import scala.annotation.tailrec

object Aoc6 extends Puzzle[IO]( 6 ) {

  case class Pos( x: Int, y: Int )

  object Pos {
    implicit val posShow: Show[Pos]     = semiauto.show[Pos]
    implicit val posMonoid: Monoid[Pos] = semiauto.monoid[Pos]
  }
  sealed abstract class Direction( val dXY: Pos ) extends EnumEntry {
    def next: Direction =
      Direction.values( (1 + Direction.values.indexOf( this )) % Direction.values.size )
  }

  object Direction extends Enum[Direction] {
    case object Up    extends Direction( Pos( 0, -1 ) )
    case object Right extends Direction( Pos( 1, 0 ) )
    case object Down  extends Direction( Pos( 0, 1 ) )
    case object Left  extends Direction( Pos( -1, 0 ) )

    override def values: Vector[Direction] = findValues.toVector
  }

  sealed abstract class Cell( val repr: Char ) extends EnumEntry {
    def toPlan: Cell.Plan = this match {
      case Cell.Empty => Cell.Empty
      case Cell.Junk  => Cell.Junk
      case Cell.Guard => Cell.Empty
    }
  }
  object Cell extends Enum[Cell] {
    sealed trait Plan extends Cell

    case object Empty extends Cell( '.' ) with Plan
    case object Junk  extends Cell( '#' ) with Plan
    case object Guard extends Cell( '^' )

    override def values: Vector[Cell] = findValues.toVector
  }

  case class Grid( guard: Pos, guardDir: Direction, byRow: Vector[Vector[Cell.Plan]] ) {
    require( byRow.nonEmpty && byRow( 0 ).nonEmpty )

    val height: Int = byRow.size
    val width: Int  = byRow( 0 ).size

    private def inBounds( pos: Pos ): Boolean =
      pos.x >= 0 && pos.y >= 0 &&
        pos.x < width && pos.y < height

    def get( pos: Pos ): Option[Cell.Plan] =
      Option.when( inBounds( pos ) )( byRow( pos.y )( pos.x ) )

    private def step: Option[( Grid, ( Pos, Direction ) )] = {
      val nextGuard: Pos = guard |+| guardDir.dXY
      get( nextGuard ).map {
        case Cell.Empty => ( copy( guard = nextGuard ), ( nextGuard, guardDir ) )
        case Cell.Junk =>
          val nextDir: Direction = guardDir.next
          ( copy( guardDir = nextDir ), ( guard, nextDir ) )
      }
    }

    @tailrec
    private def patrolRec( visited: Set[( Pos, Direction )] ): ( Set[Pos], Boolean ) = {
      step match {
        case None                                   => ( visited.map( _._1 ), false )
        case Some( ( _, into ) ) if visited( into ) => ( visited.map( _._1 ), true )
        case Some( ( grid, into ) )                 => grid.patrolRec( visited.incl( into ) )
      }
    }

    def patrol: ( Set[Pos], Boolean ) = patrolRec( Set( ( guard, guardDir ) ) )

    def createsLoop( extraJunk: Pos ): Boolean =
      copy( byRow = byRow.updated( extraJunk.y, byRow( extraJunk.y ).updated( extraJunk.x, Cell.Junk ) ) ).patrol._2

    def countLoopingObstructions: IO[Int] =
      Stream
        .emits[IO, Pos]( patrol._1.toSeq )
        .filterNot( _ == guard )
        .parEvalMapUnordered( 8 )( pos => IO.interruptible( createsLoop( pos ) ) )
        .map( b => if (b) 1 else 0 )
        .compile
        .foldMonoid

  }

  object Grid {
    def of( rows: Vector[Vector[Cell]] ): Either[String, Grid] =
      rows.zipWithIndex
        .collectFirstSome {
          case ( row, j ) =>
            row.zipWithIndex.collectFirst {
              case ( Cell.Guard, i ) => Grid( Pos( i, j ), Direction.Up, rows.map( _.map( _.toPlan ) ) )
            }
        }
        .toRight( "Guard not found" )
  }

  object parsers {
    val cell: Parser[Cell] = Cell.values.map( c => Parser.char( c.repr ).as( c ) ).foldK

    val row: Parser[Vector[Cell]] = cell.rep.map( c => c.toNev.toVector )
  }

  def parseGrid( input: Input ): Either[String, Grid] =
    input.lines
      .traverse( parsers.row.parseAll )
      .leftMap( err => s"Parser error $err" )
      .flatMap( Grid.of )

  override def run( input: Input ): IO[String] =
    parseGrid( input )
      .map( _.patrol._1.size )
      .map( _.toString )
      .leftMap( err => new RuntimeException( err ) )
      .liftTo[IO]

  override def runBonus( input: Input ): IO[String] =
    parseGrid( input )
      .leftMap( err => new RuntimeException( err ) )
      .liftTo[IO]
      .flatMap( _.countLoopingObstructions )
      .map( _.toString )
}
