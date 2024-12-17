package aoc24

import cats.Monoid
import cats.Order
import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Aoc16 extends Puzzle[Either[String, *]]( 16 ) {

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

  sealed abstract class Direction( val v: V2, val repr: Char ) extends EnumEntry {
    lazy val next: Direction = Direction.values( (Direction.values.indexOf( this ) + 1) % Direction.values.size )
    lazy val prev: Direction = Direction.values( (Direction.values.indexOf( this ) + 3) % Direction.values.size )

  }
  object Direction extends Enum[Direction] {
    case object Up    extends Direction( V2( 0, -1 ), '^' )
    case object Right extends Direction( V2( 1, 0 ), '>' )
    case object Down  extends Direction( V2( 0, 1 ), 'v' )
    case object Left  extends Direction( V2( -1, 0 ), '<' )

    override val values: Vector[Direction]              = findValues.toVector
    implicit val directionOrder: Order[Direction]       = Order.by( _.v )
    implicit val directionOrdering: Ordering[Direction] = Order.catsKernelOrderingForOrder

  }

  case class Reindeer( pos: V2, facing: Direction ) {
    def turnDirect: Reindeer    = copy( facing = facing.prev )
    def turnClockwise: Reindeer = copy( facing = facing.next )
    def move: Reindeer          = copy( pos = pos |+| facing.v )
  }

  object Reindeer {
    implicit val reindeerShow: Show[Reindeer]         = semiauto.show[Reindeer]
    implicit val reindeerOrder: Order[Reindeer]       = semiauto.order[Reindeer]
    implicit val reindeerOrdering: Ordering[Reindeer] = Order.catsKernelOrderingForOrder
  }

  case class Maze( paths: Set[V2], start: V2, goal: V2 ) {

    def step( reindeer: Reindeer ): Vector[( Reindeer, Int )] =
      Vector( reindeer.turnDirect, reindeer.turnClockwise ).tupleRight( 1000 ) ++
        Some( ( reindeer.move, 1 ) ).filter( r => paths( r._1.pos ) )

    def bestPath: Option[Int] = {

      @tailrec
      def go(
          seen: Map[Reindeer, Int],
          open: SortedSet[( Int, Reindeer )]
      ): Option[Int] =
        open.headOption match {
          case None => None
          case Some( ( c, st ) ) =>
            if (seen.get( st ).exists( _ <= c )) go( seen, open.tail )
            else {
              val next: Vector[( Reindeer, Int )] = step( st ).map( _.map( _ + c ) )
              next.collectFirst { case ( Reindeer( `goal`, _ ), cost ) => cost } match {
                case Some( cost ) => Some( cost )
                case None =>
                  val newOpen: SortedSet[( Int, Reindeer )] =
                    next.foldLeft( open ) {
                      case ( acc, ( s, x ) ) =>
                        if (seen.get( s ).exists( _ <= x )) acc
                        else acc.incl( ( x, s ) )
                    }

                  val newSeen: Map[Reindeer, Int] = seen.updated( st, c )

                  go( newSeen, newOpen )
              }
            }
        }

      go( Map.empty, SortedSet( ( 0, Reindeer( start, Direction.Right ) ) ) )
    }

    def bestPaths: Int = {

      @tailrec
      def finish( backtrack: Map[Reindeer, ( Int, Set[Reindeer] )], acc: Set[V2], next: Vector[Reindeer] ): Int =
        next.headOption match {
          case None => acc.size
          case Some( p ) =>
            finish(
              backtrack,
              acc.incl( p.pos ),
              next.drop( 1 ) ++ backtrack.get( p ).foldMap( _._2 ).filterNot( r => acc( r.pos ) )
            )
        }

      @tailrec
      def go(
          seen: Map[Reindeer, ( Int, Set[Reindeer] )],
          open: SortedSet[( Int, Reindeer, Option[Reindeer] )],
          best: Option[( Int, Reindeer )]
      ): Int = {

        def doFinish: Int = finish( seen, Set.empty, best.map( _._2 ).toVector )

        open.headOption match {
          case None => doFinish
          case Some( ( c, st, prev ) ) =>
            if (best.exists { case ( b, _ ) => b < c })
              doFinish
            else {
              val ( newSeen: Map[Reindeer, ( Int, Set[Reindeer] )], skip: Boolean ) = seen.get( st ) match {
                case Some( ( v, _ ) ) if v < c => ( seen, true )
                case Some( ( v, _ ) ) if v > c => ( seen.updated( st, ( c, prev.toSet ) ), false )
                case Some( ( _, s ) )          => ( seen.updated( st, ( c, s ++ prev ) ), true )
                case _ =>
                  if (st.pos == goal) println( show"Reached goal first: $st, $c, $prev" ) else ()
                  ( seen.updated( st, ( c, prev.toSet ) ), false )
              }

              if (skip)
                go( newSeen, open.tail, best )
              else {
                val next: Vector[( Int, Reindeer, Option[Reindeer] )] =
                  step( st ).map { case ( n, x ) => ( c + x, n, st.some ) }

                val newBest: Option[( Int, Reindeer )] =
                  best.orElse( next.collectFirst { case ( x, r @ Reindeer( `goal`, _ ), _ ) => ( x, r ) } )

                val newOpen: SortedSet[( Int, Reindeer, Option[Reindeer] )] =
                  next.foldLeft( open ) {
                    case ( acc, t @ ( x, s, _ ) ) =>
                      if (seen.get( s ).exists( _._1 < x )) acc
                      else acc.incl( t )
                  }

                go( newSeen, newOpen, newBest )
              }
            }
        }
      }

      go( Map.empty, SortedSet( ( 0, Reindeer( start, Direction.Right ), none ) ), none )
    }
  }

  object Maze {
    def parse( input: Input ): Either[String, Maze] = {
      val ( paths: Set[V2], startOpt: Option[V2], goalOpt: Option[V2] ) =
        input.lines.zipWithIndex.foldMap {
          case ( line, y ) =>
            line.zipWithIndex.toVector.foldMap {
              case ( c, x ) =>
                c match {
                  case '.' => ( Set( V2( x, y ) ), none, none )
                  case 'S' => ( Set( V2( x, y ) ), V2( x, y ).some, none )
                  case 'E' => ( Set( V2( x, y ) ), none, V2( x, y ).some )
                  case _   => ( Set.empty[V2], none, none )
                }
            }
        }

      (
        startOpt.toRight( "Start missing" ),
        goalOpt.toRight( "Goal missing" )
      ).mapN( Maze( paths, _, _ ) )
    }

  }

  override def run( input: Input ): Either[String, String] =
    Maze
      .parse( input )
      .flatMap( _.bestPath.toRight( "Not found" ) )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    Maze
      .parse( input )
      .map( _.bestPaths )
      .map( _.toString )
}
