package aoc24

import cats.data.NonEmptyVector
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import scala.annotation.tailrec

object Aoc2 extends Puzzle[Either[String, *]]( 2 ) {

  private def isSafe( levels: Vector[Int] ): Option[Int] =
    if (levels.length <= 1) None
    else {
      val firstStep: Int = levels( 1 ) - levels( 0 )
      if (safeInc( firstStep ))
        isSafeRec( levels, safeInc, 1 )
      else if (safeDec( firstStep ))
        isSafeRec( levels, safeDec, 1 )
      else
        Some( 0 )
    }

  private def safeInc( step: Int ): Boolean = step >= 1 && step <= 3
  private def safeDec( step: Int ): Boolean = step >= -3 && step <= -1

  @tailrec
  private def isSafeRec( levels: Vector[Int], stepSafe: Int => Boolean, from: Int ): Option[Int] =
    if (from >= levels.length - 1) None
    else {
      val step: Int = levels( from + 1 ) - levels( from )
      if (stepSafe( step ))
        isSafeRec( levels, stepSafe, from + 1 )
      else
        Some( from )
    }

  case class Report( levels: NonEmptyVector[Int] ) {
    def isSafe: Boolean = Aoc2.isSafe( levels.toVector ).isEmpty

    def isSafeWithDampener: Boolean = Aoc2.isSafe( levels.toVector ) match {
      case None => true
      case Some( problemAt ) =>
        val evicted: Set[Int] = Set( 0, 1, problemAt, problemAt + 1 )
        evicted.exists( ix => Aoc2.isSafe( levels.toVector.patch( ix, Vector.empty, 1 ) ).isEmpty )
    }
  }

  val reportParser: Parser[Report] =
    Numbers.nonNegativeIntString
      .mapFilter( _.toIntOption )
      .repSep( Rfc5234.wsp.rep.void )
      .map( nel => Report( nel.toNev ) )

  private def readReports( input: Input ): Either[String, Vector[Report]] =
    input.lines
      .traverse( reportParser.parseAll )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    readReports( input )
      .map( reports => reports.count( _.isSafe ) )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    readReports( input )
      .map( reports => reports.count( _.isSafeWithDampener ) )
      .map( _.toString )
}
