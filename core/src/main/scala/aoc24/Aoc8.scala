package aoc24

import cats.Group
import cats.Show
import cats.data.NonEmptyList
import cats.effect.IO
import cats.parse.Parser
import cats.syntax.all._
import scala.annotation.tailrec

object Aoc8 extends Puzzle[IO]( 8 ) {

  case class Pos( x: Int, y: Int ) { self =>
    def step( v: Pos ): Iterator[Pos] =
      new Iterator[Pos] {
        var c: Pos = self

        override def hasNext: Boolean = true

        override def next(): Pos = {
          c = c |+| v
          c
        }
      }
  }

  object Pos {
    implicit val posShow: Show[Pos] = Show { case Pos( x, y ) => s"($x, $y)" }
    implicit val posMonoid: Group[Pos] = new Group[Pos] {
      override def inverse( p: Pos ): Pos = Pos( -p.x, -p.y )

      override def empty: Pos = Pos( 0, 0 )

      override def combine( p: Pos, q: Pos ): Pos = Pos( p.x + q.x, p.y + q.y )
    }

    def reducedVector( p: Pos, q: Pos ): Pos = {
      val v: Pos = q.remove( p )
      val g: Int = gcd( v.x, v.y )
      Pos( v.x / g, v.y / g )
    }
  }

  @tailrec
  private def gcd( a: Int, b: Int ): Int =
    if (a < 0 || b < 0)
      gcd( a.abs, b.abs )
    else if (a < b)
      gcdRec( b, a )
    else
      gcdRec( a, b )

  @tailrec
  private def gcdRec( a: Int, b: Int ): Int =
    if (b == 0) a else gcdRec( b, a % b )

  case class Antennas(
      height: Int,
      width: Int,
      antennas: Vector[( Pos, Char )]
  ) {
    def isInBounds( pos: Pos ): Boolean =
      pos.x >= 0 && pos.x < width &&
        pos.y >= 0 && pos.y < height

    def antennaPairs: Map[Char, IndexedSeq[( Pos, Pos )]] =
      antennas
        .groupMap( _._2 )( _._1 )
        .fmap(
          positions =>
            for {
              i <- positions.indices
              j <- positions.indices if i != j
            } yield ( positions( i ), positions( j ) )
        )

    def antiNodes: Set[Pos] =
      antennaPairs
        .unorderedFoldMap( pairs => pairs.map { case ( p, q ) => p |+| p.remove( q ) }.toSet )
        .filter( isInBounds )

    def antennaPairsU: Map[Char, IndexedSeq[( Pos, Pos )]] =
      antennas
        .groupMap( _._2 )( _._1 )
        .fmap(
          positions =>
            for {
              i <- positions.indices
              j <- positions.indices.drop( i + 1 )
            } yield ( positions( i ), positions( j ) )
        )

    def allAntiNodesOf( p: Pos, q: Pos ): Set[Pos] = {
      val v: Pos             = Pos.reducedVector( p, q )
      val fwd: Iterator[Pos] = p.step( v ).takeWhile( isInBounds )
      val bwd: Iterator[Pos] = p.step( v.inverse() ).takeWhile( isInBounds )

      Set( p ) ++ fwd ++ bwd
    }

    def allAntiNodes: Set[Pos] =
      antennaPairsU
        .unorderedFoldMap( pairs => pairs.toVector.foldMap { case ( p, q ) => allAntiNodesOf( p, q ) } )

    def showAllAntiNodes: String = {
      val antiNodesSet: Set[Pos]        = allAntiNodes
      val antennasByPos: Map[Pos, Char] = antennas.toMap
      0.until( height )
        .map(
          j =>
            0.until( width )
              .map { i =>
                val p = Pos( i, j )
                antennasByPos.getOrElse( p, if (antiNodesSet( p )) '#' else '.' )
              }
              .mkString
        )
        .mkString( "\n" )
    }

  }

  object parsers {
    val empty: Parser[Option[Char]]     = Parser.char( '.' ).as( None )
    val frequency: Parser[Option[Char]] = Parser.charWhere( _.isLetterOrDigit ).map( Some( _ ) )

    val row: Parser[NonEmptyList[Option[Char]]] = (empty | frequency).rep
  }

  private def parseAntennas( input: Input ): IO[Antennas] =
    input.lines.toNev
      .liftTo[IO]( new RuntimeException( "No rows" ) )
      .flatMap(
        _.traverse( parsers.row.parseAll )
          .leftMap( err => new RuntimeException( s"Parser error $err" ) )
          .liftTo[IO]
      )
      .map { rows =>
        Antennas(
          rows.length,
          rows.head.length,
          rows.zipWithIndex.foldMap {
            case ( row, j ) =>
              row.zipWithIndex.foldMap {
                case ( antennaOpt, i ) => antennaOpt.tupleLeft( Pos( i, j ) ).toVector
              }
          }
        )
      }

  override def run( input: Input ): IO[String] =
    parseAntennas( input )
      .map( _.antiNodes.size )
      .map( _.toString )

  override def runBonus( input: Input ): IO[String] =
    parseAntennas( input )
      .map( _.allAntiNodes.size )
      .map( _.toString )
}
