package aoc24

import cats.data.NonEmptyVector
import cats.effect.IO
import cats.parse.Parser
import cats.syntax.all._
import fs2.Stream

object Aoc10 extends Puzzle[IO]( 10 ) {

  case class Pos( x: Int, y: Int ) {
    def left: Pos               = Pos( x - 1, y )
    def right: Pos              = Pos( x + 1, y )
    def up: Pos                 = Pos( x, y - 1 )
    def down: Pos               = Pos( x, y + 1 )
    def neighbours: Vector[Pos] = Vector( left, up, right, down )
  }

  case class Grid( rows: NonEmptyVector[NonEmptyVector[Int]] ) {
    val height: Int = rows.length
    val width: Int  = rows.head.length

    val trailheads: Vector[Pos] =
      rows.zipWithIndex
        .foldMap { case ( row, y ) => row.zipWithIndex.collect { case ( 0, x ) => Pos( x, y ) } }

    def get( pos: Pos ): Option[Int] =
      rows.toVector.lift( pos.y ).flatMap( _.toVector.lift( pos.x ) )

    private def neighbours( p: Pos, v: Int ): Vector[( Pos, Int )] =
      p.neighbours.mapFilter( q => get( q ).filter( _ == v + 1 ).tupleLeft( q ) )

    // part 1
    def scoreRec( reached: Set[Pos], seen: Set[Pos], from: Vector[( Pos, Int )] ): Int =
      from match {
        case ( p, v ) +: ps =>
          if (seen( p ))
            scoreRec( reached, seen, ps )
          else if (v == 9)
            scoreRec( reached.incl( p ), seen.incl( p ), ps )
          else {
            val next: Vector[( Pos, Int )] = neighbours( p, v )
            scoreRec( reached, seen.incl( p ), ps ++ next )
          }
        case _ => reached.size
      }

    def scores: IO[Int] =
      Stream
        .emits[IO, Pos]( trailheads )
        .parEvalMapUnordered( 8 )( th => IO.delay( scoreRec( Set.empty, Set.empty, Vector( ( th, 0 ) ) ) ) )
        .compile
        .foldMonoid

    // part 2

    def ratingRec( pathsBack: Map[Pos, Int], from: Vector[( Pos, Option[Pos], Int )] ): Int =
      from match {
        case ( p, pp, v ) +: ps =>
          val pb: Int = pp.flatMap( pathsBack.get ).getOrElse( 1 )

          pathsBack.get( p ) match {
            case Some( np ) =>
              ratingRec( pathsBack.updated( p, np + pb ), ps )
            case None if v == 9 =>
              ratingRec( pathsBack.updated( p, pb ), ps )
            case None =>
              ratingRec(
                pathsBack.updated( p, pb ),
                ps ++ neighbours( p, v ).map { case ( q, w ) => ( q, p.some, w ) }
              )
          }

        case _ =>
          pathsBack.filter { case ( p, _ ) => get( p ).contains( 9 ) }.unorderedFold
      }

    def ratings: IO[Int] =
      Stream
        .emits[IO, Pos]( trailheads )
        .parEvalMapUnordered( 8 )( th => IO.delay( ratingRec( Map.empty, Vector( ( th, None, 0 ) ) ) ) )
        .compile
        .foldMonoid

  }

  object parsers {
    val height: Parser[Int]              = Parser.charWhere( c => c >= '0' && c <= '9' ).map( _ - '0' )
    val row: Parser[NonEmptyVector[Int]] = height.rep.map( _.toNev )
  }

  /*private*/
  def parseGrid( input: Input ): IO[Grid] =
    input.lines.toNev
      .liftTo[IO]( new RuntimeException( "no rows in input" ) )
      .flatMap(
        _.traverse(
          r => parsers.row.parseAll( r ).leftMap( err => new RuntimeException( s"Parser error $err" ) ).liftTo[IO]
        )
      )
      .map( Grid )

  override def run( input: Input ): IO[String] =
    parseGrid( input )
      .flatMap( _.scores )
      .map( _.toString )

  override def runBonus( input: Input ): IO[String] =
    parseGrid( input )
      .flatMap( _.ratings )
      .map( _.toString )
}
