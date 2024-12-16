package aoc24

import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

object Aoc15 extends Puzzle[Either[String, *]]( 15 ) {

  case class V2( x: Int, y: Int ) {
    override def toString: String = s"($x, $y)"
  }

  object V2 {
    implicit val v2Monoid: Monoid[V2] = semiauto.monoid[V2]
    val zero: V2                      = v2Monoid.empty
    implicit val v2Show: Show[V2]     = Show.fromToString
  }

  abstract class Grid { self =>
    def crates: Set[V2]
    def robot: V2

    def move( dir: Direction ): Grid

    @tailrec
    final def moveAll( dirs: List[Direction] ): Grid =
      dirs match {
        case Nil          => this
        case head :: next => move( head ).moveAll( next )
      }

    def crateCoordinates: Int =
      crates.unorderedFoldMap( c => c.x + 100 * c.y )
  }

  case class NarrowGrid( width: Int, height: Int, walls: Set[V2], crates: Set[V2], robot: V2 ) extends Grid {
    /*
     * Some(None): move, no crates moved
     * Some(Some(p)): move, push crates between robot and p
     */
    override def move( dir: Direction ): NarrowGrid = {
      val target: V2 = robot |+| dir.v
      @tailrec
      def go( p: V2, crate: Boolean ): NarrowGrid = {
        // wall: cannot move
        if (walls( p )) this
        // crate: record the next position of the farthest crate do far
        else if (crates( p )) {
          val next: V2 = p |+| dir.v
          go( next, crate = true )
        } else
          // empty: move the robot, and move the crates seen so far
          copy(
            crates = crates.excl( target ) ++ Option.when( crate )( p ),
            robot = target
          )
      }

      go( target, crate = false )
    }

  }

  case class WideGrid( width: Int, height: Int, walls: Set[V2], crates: Set[V2], robot: V2 ) extends Grid {

    // looking at a point, None if empty/wall, Some((create, next)) if crate
    type CrateSearch = V2 => Option[( V2, Vector[V2] )]

    val crateSearchLeft: CrateSearch = (p: V2) => {
      val look: V2 = p |+| Direction.Left.v
      Option.when( crates( look ) )( ( look, Vector( look |+| Direction.Left.v ) ) )
    }

    val crateSearchRight: CrateSearch = (p: V2) => {
      Option.when( crates( p ) )( ( p, Vector( p |+| Direction.Right.v |+| Direction.Right.v ) ) )
    }

    def crateSearchVert( dir: Direction ): CrateSearch = (p: V2) => {
      def at( q: V2 ): Option[( V2, Vector[V2] )] =
        Option.when( crates( q ) )( ( q, Vector( q |+| dir.v, q |+| Direction.Right.v |+| dir.v ) ) )
      at( p ).orElse( at( p |+| Direction.Left.v ) )
    }

    def moveDir( d: V2, search: CrateSearch ): WideGrid = {
      val target: V2 = robot |+| d

      @tailrec
      def go( searched: Vector[V2], acc: Vector[V2] ): WideGrid =
        searched.foldMapM(
          p =>
            Option.when( !walls( p ) )( search( p ).foldMap {
              case ( crate, searchedNext ) =>
                //println( show"search $p, found $crate, then searching ${searchedNext.mkString_( ", " )}" )
                ( Set( crate ), searchedNext.toSet )
            } )
        ) match {
          case None => this
          case Some( ( moved, searchedNext ) ) =>
            if (moved.isEmpty) {
              copy(
                robot = target,
                crates = crates.removedAll( acc ) ++ acc.map( _ |+| d )
              )
            } else go( searchedNext.toVector, acc ++ moved )
        }

      go( Vector( target ), Vector.empty )
    }

    override def move( dir: Direction ): Grid = dir match {
      case d @ Direction.Left  => moveDir( d.v, crateSearchLeft )
      case d @ Direction.Right => moveDir( d.v, crateSearchRight )
      case d                   => moveDir( d.v, crateSearchVert( d ) )
    }
  }

  object WideGrid {
    private def doubleX( p: V2 ): V2 = V2( 2 * p.x, p.y )

    def of( narrow: NarrowGrid ): WideGrid =
      WideGrid(
        narrow.width * 2,
        narrow.height,
        narrow.walls.unorderedFoldMap { w =>
          val lhs: V2 = doubleX( w )
          Set( lhs, lhs |+| V2( 1, 0 ) )
        },
        narrow.crates.map( doubleX ),
        doubleX( narrow.robot )
      )
  }

  sealed abstract class Direction( val v: V2, val repr: Char ) extends EnumEntry
  object Direction extends Enum[Direction] {
    case object Up    extends Direction( V2( 0, -1 ), '^' )
    case object Down  extends Direction( V2( 0, 1 ), 'v' )
    case object Left  extends Direction( V2( -1, 0 ), '<' )
    case object Right extends Direction( V2( 1, 0 ), '>' )

    override val values: Vector[Direction] = findValues.toVector
  }

  object parsers {

    def endl: Parser[Unit] = Rfc5234.lf | (Rfc5234.cr ~ Rfc5234.lf.rep0).void

    case class Row( width: Int, walls: Set[V2], crates: Set[V2], robot: Option[V2] )

    def row( y: Int ): Parser[Row] =
      ( 0, Set.empty[V2], Set.empty[V2], none[V2] )
        .tailRecM {
          case ( x, walls, crates, robot ) =>
            Parser.char( '#' ).as( Left( ( x + 1, walls.incl( V2( x, y ) ), crates, robot ) ) ) |
              Parser.char( 'O' ).as( Left( ( x + 1, walls, crates.incl( V2( x, y ) ), robot ) ) ) |
              Parser.char( '@' ).as( Left( ( x + 1, walls, crates, Some( V2( x, y ) ) ) ) ) |
              Parser.char( '.' ).as( Left( ( x + 1, walls, crates, robot ) ) ) |
              endl.as( Right( Row( x, walls, crates, robot ) ) )
        }
        .filter( _.width > 0 )

    def grid: Parser[Option[NarrowGrid]] =
      ( 0, 0, Set.empty[V2], Set.empty[V2], none[V2] ).tailRecM {
        case ( y, width, walls, crates, robotOpt ) =>
          row( y ).map {
            case Row( rowWidth, rowWalls, rowCrates, rowRobotOpt ) =>
              if (width > 0 && width != rowWidth)
                Either.right( none )
              else if (robotOpt.isDefined && rowRobotOpt.isDefined)
                Either.right( none )
              else
                Either.left(
                  (
                    y + 1,
                    rowWidth,
                    walls.union( rowWalls ),
                    crates.union( rowCrates ),
                    robotOpt.orElse( rowRobotOpt )
                  )
                )
          }.backtrack |
            endl.as(
              robotOpt
                .filter( _ => y > 0 && width > 0 )
                .traverse { rob =>
                  Either.right( NarrowGrid( width, y, walls, crates, rob ) )
                }
            )
      }

    val move: Parser[Direction] =
      Direction.values.map( m => Parser.char( m.repr ).as( m ) ).foldK
    val moves: Parser[List[Direction]] =
      move.rep.repSep( endl ).map( _.flatten ).map( _.toList )

    val gridAndInstructions: Parser[( Option[NarrowGrid], List[Direction] )] =
      (grid ~ moves) <* endl.rep0
  }

  def parseGridAndInstructions( input: Input ): Either[String, ( NarrowGrid, List[Direction] )] =
    parsers.gridAndInstructions
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )
      .flatMap {
        case ( None, _ )              => Left( "Invalid grid" )
        case ( Some( grid ), instrs ) => Right( ( grid, instrs ) )
      }

  override def run( input: Input ): Either[String, String] =
    parseGridAndInstructions( input )
      .map { case ( grid, instrs ) => grid.moveAll( instrs ).crateCoordinates }
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseGridAndInstructions( input )
      .map { case ( grid, instrs ) => WideGrid.of( grid ).moveAll( instrs ).crateCoordinates }
      .map( _.toString )
}
