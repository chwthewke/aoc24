package aoc24

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import scala.annotation.tailrec

object Aoc5 extends Puzzle[Either[String, *]]( 5 ) {
  case class Instructions(
      orderingRules: Set[( Int, Int )],
      updates: Vector[NonEmptyList[Int]]
  ) {
    private def isUpdateHeadValid( update: NonEmptyList[Int] ): Boolean =
      update.tail.forall( x => !orderingRules.contains( ( x, update.head ) ) )

    @tailrec
    final def isUpdateValid( update: NonEmptyList[Int] ): Boolean =
      if (!isUpdateHeadValid( update )) false
      else
        NonEmptyList.fromList( update.tail ) match {
          case Some( next ) => isUpdateValid( next )
          case None         => true
        }

    def fixHead( update: NonEmptyList[Int] ): NonEmptyList[Int] =
      update.tail match {
        case x :: xs if orderingRules.contains( ( x, update.head ) ) =>
          NonEmptyList( x, update.head :: xs )
        case _ => update
      }

    @tailrec
    final def fixOnce( acc: List[Int], update: NonEmptyList[Int] ): NonEmptyList[Int] = {
      val u: NonEmptyList[Int] = fixHead( update )
      u.tail match {
        case head :: next => fixOnce( u.head :: acc, NonEmptyList( head, next ) )
        case Nil          => NonEmptyList.ofInitLast( acc.reverse, u.head )
      }
    }

    @tailrec
    private def fixUpdateRec( steps: Int, update: NonEmptyList[Int] ): Option[NonEmptyList[Int]] = {
      if (steps == 0) None
      else {
        val fixedOnce: NonEmptyList[Int] = fixOnce( Nil, update )
        if (fixedOnce == update) Some( update )
        else fixUpdateRec( steps - 1, fixedOnce )
      }
    }

    def fixUpdate( update: NonEmptyList[Int] ): Option[NonEmptyList[Int]] =
      fixUpdateRec( update.length, update )

  }

  def middlePageNumber( update: NonEmptyVector[Int] ): Int =
    Option.when( update.length % 2 == 1 )( update.toVector( update.length / 2 ) ).getOrElse( 0 )

  object parsers {
    val pageNum: Parser[Int]               = Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
    val orderingRule: Parser[( Int, Int )] = ( pageNum, Parser.char( '|' ), pageNum ).mapN( ( x, _, y ) => ( x, y ) )
    val update: Parser[NonEmptyList[Int]]  = pageNum.repSep( Parser.char( ',' ) )

    val endl: Parser[Unit] = Rfc5234.crlf | Rfc5234.lf
    val instructions: Parser[Instructions] =
      ( orderingRule.repSep( endl ), endl.rep( 2 ), update.repSep( endl ) ).mapN(
        ( rules, _, updates ) => Instructions( rules.toNev.toVector.toSet, updates.toNev.toVector )
      ) <* endl.rep0
  }

  private def parseInstructions( input: Input ): Either[String, Instructions] =
    parsers.instructions
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    parseInstructions( input )
      .map(
        instructions =>
          instructions.updates.filter( instructions.isUpdateValid ).foldMap( u => middlePageNumber( u.toNev ) )
      )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseInstructions( input )
      .flatMap(
        instructions =>
          instructions.updates
            .filter( !instructions.isUpdateValid( _ ) )
            .traverse(
              invalid => instructions.fixUpdate( invalid ).toRight( s"Could not fix ${invalid.mkString_( ", " )}" )
            )
      )
      .map( fixed => fixed.foldMap( u => middlePageNumber( u.toNev ) ) )
      .map( _.toString )
}
