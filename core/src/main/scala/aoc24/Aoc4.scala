package aoc24

import cats.syntax.all._

object Aoc4 extends Puzzle.Pure( 4 ) {

  case class Needle( str: String ) {
    val length: Int            = str.length
    val chars: Vector[Char]    = str.toVector
    val revChars: Vector[Char] = chars.reverse
  }

  case class Haystack( table: Vector[Vector[Char]] ) {
    def transpose: Haystack = Haystack( table.transpose )
    def flip: Haystack      = Haystack( table.reverse )

    def slidingDiag( size: Int ): Iterator[Vector[Char]] =
      table
        .sliding( size )
        .collect {
          case rowsWindow if rowsWindow.size == size =>
            rowsWindow.transpose.sliding( size ).collect {
              case square if square.size == size =>
                ( 0 until size ).map( i => square( i )( i ) ).toVector
            }
        }
        .flatten

  }

  object Haystack {
    def apply( input: Input ): Haystack = Haystack( input.lines.map( _.toVector ) )
  }

  def countNeedleOccurrences( needle: Needle, windows: Iterator[Vector[Char]] ): Int =
    windows.count( w => w == needle.chars || w == needle.revChars )

  def horzOccurrences( needle: Needle, haystack: Haystack ): Int =
    haystack.table.foldMap( r => countNeedleOccurrences( needle, r.sliding( 4 ) ) )

  def vertOccurrences( needle: Needle, haystack: Haystack ): Int =
    horzOccurrences( needle, haystack.transpose )

  def diagOccurrences( needle: Needle, haystack: Haystack ): Int =
    countNeedleOccurrences( needle: Needle, haystack.slidingDiag( 4 ) )

  def invDiagOccurrences( needle: Needle, haystack: Haystack ): Int =
    countNeedleOccurrences( needle: Needle, haystack.flip.slidingDiag( 4 ) )

  override def run( input: Input ): String = {
    val haystack: Haystack = Haystack( input )
    val needle: Needle     = Needle( "XMAS" )

    val result: Int =
      horzOccurrences( needle, haystack ) +
        vertOccurrences( needle, haystack ) +
        diagOccurrences( needle, haystack ) +
        invDiagOccurrences( needle, haystack )

    result.toString
  }

  case class Grid( lines: Vector[String] ) {
    val height: Int = lines.size
    val width: Int  = lines( 0 ).length

    private def isMas( chars: Vector[Char] ): Boolean =
      chars == "MAS".toVector || chars == "SAM".toVector

    private def crossDiag1( i: Int, j: Int ): Vector[Char] =
      Vector(
        lines( j - 1 )( i - 1 ),
        lines( j )( i ),
        lines( j + 1 )( i + 1 )
      )

    private def crossDiag2( i: Int, j: Int ): Vector[Char] =
      Vector(
        lines( j - 1 )( i + 1 ),
        lines( j )( i ),
        lines( j + 1 )( i - 1 )
      )

    def xmasAt( i: Int, j: Int ): Boolean = {
      if (j <= 0 || j >= height - 1) false
      else if (i <= 0 || i >= width - 1) false
      else if (lines( j )( i ) != 'A') false
      else isMas( crossDiag1( i, j ) ) && isMas( crossDiag2( i, j ) )
    }

    def countXmases: Int = {
      val indices: Iterable[( Int, Int )] = for {
        j <- 1 until ( height - 1 )
        i <- 1 until ( width - 1 )
      } yield ( i, j )

      indices.count { case ( i, j ) => xmasAt( i, j ) }
    }
  }

  object Grid {
    def apply( input: Input ): Grid = Grid( input.lines )
  }

  override def runBonus( input: Input ): String =
    Grid( input ).countXmases.toString
}
