package aoc24

import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object Aoc9 extends Puzzle[Either[String, *]]( 9 ) {

  case class File( id: Int, size: Int )
  case class Free( size: Int )

  case class Layout(
      size: Int,
      filePositions: Vector[( Int, File )],
      filesByPosition: SortedMap[Int, File]
  ) {

    def at( ix: Int ): Option[File] =
      filesByPosition
        .maxBefore( ix + 1 )
        .collect { case ( p, f ) if ix < p + f.size => f }

    @tailrec
    final def compactRec( curF: Int, curB: Int, acc: Long ): Long = {
      def addToChecksum( file: File ): Long = acc + file.id.toLong * curF.toLong

      if (curB < curF) acc
      else
        at( curF ) match {
          case Some( file ) => compactRec( curF + 1, curB, addToChecksum( file ) )
          case None =>
            at( curB ) match {
              case Some( file ) => compactRec( curF + 1, curB - 1, addToChecksum( file ) )
              case None         => compactRec( curF, curB - 1, acc )
            }
        }
    }

    def compactAndChecksum: Long =
      compactRec( 0, size - 1, 0L )

    // part 2

    def move( ix: Int, to: Int ): Layout = {
      val ( from: Int, file: File ) = filePositions( ix )

      Layout(
        size,
        filePositions.updated( ix, ( to, file ) ),
        filesByPosition.removed( from ).updated( to, file )
      )
    }

    def getFreeSpaces: Iterator[( Int, Int )] =
      filesByPosition.tail.iterator
        .scanLeft( ( ( 0, 0 ), filesByPosition.head._2.size ) ) {
          case ( ( _, ps ), ( p, f ) ) => ( ( ps, p - ps ), p + f.size )
        }
        .drop( 1 )
        .map( _._1 )
        .filter( _._2 > 0 )

    val freeSpaces: LazyList[( Int, Int )] = LazyList.from( getFreeSpaces )

    @tailrec
    final def compactWholeFilesRec( ix: Int ): Layout =
      if (ix == 0)
        this
      else {
        val ( q, file ) = filePositions( ix )
        freeSpaces
          .takeWhile { case ( p, _ ) => p < q }
          .find { case ( _, s ) => s >= file.size } match {
          case Some( ( p, _ ) ) => move( ix, p ).compactWholeFilesRec( ix - 1 )
          case None             => compactWholeFilesRec( ix - 1 )
        }
      }

    def compactWholeFiles: Layout =
      compactWholeFilesRec( filePositions.size - 1 )

    def checksum: Long =
      filePositions
        .foldMap {
          case ( p, File( id, s ) ) =>
            id.toLong * ((s.toLong * (2 * p.toLong + s.toLong - 1)) / 2)
        }

    def compactWholeFilesAndChecksum: Long =
      compactWholeFiles.checksum

    def toStringSimple: String =
      0.until( size )
        .map { ix =>
          at( ix ) match {
            case Some( value ) => value.id.toString
            case None          => "."
          }
        }
        .mkString
  }

  object Layout {
    def apply( head: File, tail: Vector[( Free, File )] ): Layout = {
      val size: Int = head.size + tail.foldMap { case ( ff, f ) => ff.size + f.size }

      val filePositions: Vector[( Int, File )] =
        tail.scanLeft( ( 0, head ) ) {
          case ( ( p, f1 ), ( ff, f2 ) ) => ( p + f1.size + ff.size, f2 )
        }

      val filesByPosition: SortedMap[Int, File] =
        SortedMap.empty[Int, File] ++ filePositions

      Layout( size, filePositions, filesByPosition )
    }
  }

  object parsers {
    val digit: Parser[Int] = Numbers.digit.map( c => c - '0' )

    def file( id: Int ): Parser[File] = digit.map( File( id, _ ) )

    val free: Parser[Free] = digit.map( Free )

    val layout: Parser[Layout] =
      (
        file( 0 ) ~
          ( 1, List.empty[( Free, File )] ).tailRecM {
            case ( n, acc ) =>
              Parser.end
                .as( Right( acc.toVector.reverse ) )
                .orElse(
                  ( free, file( n ) ).mapN( ( ff, f ) => Left( ( n + 1, ( ff, f ) :: acc ) ) )
                )
          }
      ).map { case ( f0, fs ) => Layout( f0, fs ) }
  }

  private def parseLayout( input: Input ): Either[String, Layout] =
    parsers.layout
      .parseAll( input.trimmed )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    parseLayout( input )
      .map( _.compactAndChecksum )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseLayout( input )
      .map( _.compactWholeFilesAndChecksum )
      .map( _.toString )
}
