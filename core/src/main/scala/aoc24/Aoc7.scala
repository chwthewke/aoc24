package aoc24

import cats.data.NonEmptyList
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry

object Aoc7 extends Puzzle[Either[String, *]]( 7 ) {
  case class Operand( value: Long, nextPowerOf10: Long )
  object Operand {
    def of( nonNegativeIntString: String ): Option[Operand] =
      ( nonNegativeIntString.toLongOption, ("1" + "0" * nonNegativeIntString.length).toLongOption )
        .mapN( Operand( _, _ ) )
  }

  sealed abstract class Operator( val apply: ( Long, Operand ) => Long ) extends EnumEntry
  object Operator extends Enum[Operator] {
    case object Add    extends Operator( ( x, y ) => x + y.value )
    case object Mul    extends Operator( ( x, y ) => x * y.value )
    case object Concat extends Operator( ( x, y ) => x * y.nextPowerOf10 + y.value )

    override val values: Vector[Operator] = findValues.toVector

    val part1Operators: Vector[Operator] = Vector( Add, Mul )
    val part2Operators: Vector[Operator] = values
  }

  case class Equation( result: Long, operands: NonEmptyList[Operand] ) {

    def findOperators( from: Vector[Operator] ): Option[List[Operator]] =
      findOperatorsRec( from, operands.head.value, Nil, operands.tail )

    final def findOperatorsRec(
        from: Vector[Operator],
        acc: Long,
        ops: List[Operator],
        remOperands: List[Operand]
    ): Option[List[Operator]] = {
      if (acc > result) None
      else
        remOperands match {
          case Nil => Option.when( acc == result )( ops.reverse )
          case h :: t =>
            from.collectFirstSome( op => findOperatorsRec( from, op.apply( acc, h ), op :: ops, t ) )
        }
    }

  }

  object parsers {
    val number: Parser[Long]     = Numbers.nonNegativeIntString.mapFilter( _.toLongOption )
    val operand: Parser[Operand] = Numbers.nonNegativeIntString.mapFilter( Operand.of )

    val equation: Parser[Equation] =
      ( number, Parser.char( ':' ) *> Rfc5234.wsp.rep0, operand.repSep( Rfc5234.wsp.rep ) )
        .mapN( ( r, _, os ) => Equation( r, os ) )
  }

  private def parseEquations( input: Input ): Either[String, Vector[Equation]] =
    input.lines
      .traverse( parsers.equation.parseAll )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    parseEquations( input )
      .map( _.mapFilter( eq => eq.findOperators( Operator.part1Operators ).as( eq ) ).foldMap( _.result ) )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseEquations( input )
      .map( _.mapFilter( eq => eq.findOperators( Operator.part2Operators ).as( eq ) ).foldMap( _.result ) )
      .map( _.toString )

}
