package aoc24

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.Kleisli
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Aoc20 extends Puzzle[Kleisli[Either[String, *], IsSample, *]]( 20 ) {
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

  sealed abstract class Direction( val v: V2 ) extends EnumEntry

  object Direction extends Enum[Direction] {
    case object Up    extends Direction( V2( 0, -1 ) )
    case object Right extends Direction( V2( 1, 0 ) )
    case object Down  extends Direction( V2( 0, 1 ) )
    case object Left  extends Direction( V2( -1, 0 ) )

    override val values: Vector[Direction]              = findValues.toVector
    implicit val directionOrder: Order[Direction]       = Order.by( _.v )
    implicit val directionOrdering: Ordering[Direction] = Order.catsKernelOrderingForOrder
  }

  case class Maze( width: Int, height: Int, paths: Set[V2], start: V2, goal: V2 ) {

    def neighbours( state: TraversalState ): Vector[TraversalState] =
      Direction.values
        .map( d => state.pos |+| d.v )
        .mapFilter( p => Option.when( paths( p ) )( TraversalState( p, state.elapsed + 1 ) ) )

    def cheats2Pico( state: TraversalState ): Vector[V2] =
      Direction.values
        .map( _.v )
        .mapFilter { d =>
          val p: V2 = state.pos |+| d
          val q: V2 = p |+| d
          Option.when( !paths( p ) && paths( q ) )( q )
        }

    def cheats20Pico( state: TraversalState ): Vector[( V2, Int )] =
      ( for {
        dx <- -20 to 20
        dy <- -( 20 - dx.abs ) to ( 20 - dx.abs )
        p = V2( state.pos.x + dx, state.pos.y + dy ) if paths( p )
      } yield ( p, dx.abs + dy.abs ) ).toVector

    val part1CheatRule: TraversalState => Vector[( V2, Int )] = cheats2Pico( _ ).tupleRight( 2 )

    val part2CheatRule: TraversalState => Vector[( V2, Int )] = cheats20Pico

    def run( cheatRule: TraversalState => Vector[( V2, Int )] ): SortedMap[Int, Int] = {

      def finish( cheats: Map[( V2, V2 ), Int] ): SortedMap[Int, Int] = {
//        println(
//          cheats.toVector
//            .sortBy { case ( ( f, t ), d ) => ( -d, f, t ) }
//            .map { case ( ( from, to ), adv ) => show"$from->$to gains $adv ps" }
//            .mkString( "cheats\n  ", "\n  ", "" )
//        )

        cheats.unorderedFoldMap( adv => SortedMap( adv -> 1 ) )
      }

      @tailrec
      def go(
          seen: Set[V2],
          open: Vector[TraversalState],
          futureCheats: Map[V2, NonEmptyVector[( V2, Int )]], // (to, (from, elapsed@to))
          validCheats: Map[( V2, V2 ), Int]                   // ((from, to), gain)
      ): SortedMap[Int, Int] =
        open.headOption match {
          case None => finish( validCheats )
          case Some( state ) =>
            if (seen.contains( state.pos ))
              go( seen, open.tail, futureCheats, validCheats )
            else {
              val nextSeen: Set[V2]                = seen.incl( state.pos )
              val nextOpen: Vector[TraversalState] = open.tail ++ neighbours( state )
              val newFutureCheats: Map[V2, NonEmptyVector[( V2, Int )]] =
                cheatRule( state )
                  .filterNot { case ( to, _ ) => seen( to ) }
                  .foldMap { case ( to, l ) => Map( to -> NonEmptyVector.one( ( state.pos, state.elapsed + l ) ) ) }
              val newValidCheats =
                futureCheats
                  .get( state.pos )
                  .foldMap( cs => cs.toVector.map { case ( from, el ) => ( ( from, state.pos ), state.elapsed - el ) } )
                  .toMap

              go(
                nextSeen,
                nextOpen,
                futureCheats.removed( state.pos ) |+| newFutureCheats,
                validCheats ++ newValidCheats
              )

            }
        }

      go( Set.empty, Vector( TraversalState( start, 0 ) ), Map.empty, Map.empty )
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
      ).mapN( Maze( input.lines( 0 ).length, input.lines.size, paths, _, _ ) )
    }

  }

  case class TraversalState( pos: V2, elapsed: Int )

  def part1Threshold( isSample: IsSample ): Int =
    if (isSample.value) 1 else 100

  def part2Threshold( isSample: IsSample ): Int =
    if (isSample.value) 50 else 100

  def countCheatsBetterThanThreshold(
      maze: Maze,
      threshold: Int,
      cheatRule: TraversalState => Vector[( V2, Int )]
  ): Int =
    maze.run( cheatRule ).rangeFrom( threshold ).combineAll

  override def run( input: Input ): Kleisli[Either[String, *], IsSample, String] =
    Kleisli( isSample =>
      Maze
        .parse( input )
        .map( maze => countCheatsBetterThanThreshold( maze, part1Threshold( isSample ), maze.part1CheatRule ) )
        .map( _.toString )
    )

  override def runBonus( input: Input ): Kleisli[Either[String, *], IsSample, String] =
    Kleisli( isSample =>
      Maze
        .parse( input )
        .map( maze => countCheatsBetterThanThreshold( maze, part2Threshold( isSample ), maze.part2CheatRule ) )
        .map( _.toString )
    )
}
