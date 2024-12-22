package aoc24

import cats.data.Chain
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

object Aoc17 extends Puzzle[Either[String, *]]( 17 ) {

  case class Machine(
      registerA: BigInt,
      registerB: BigInt,
      registerC: BigInt,
      instrPointer: Int,
      program: Vector[Int],
      log: Chain[BigInt],
      loop: Boolean = true
  ) {
    def combo( code: Int ): BigInt =
      code match {
        case 4 => registerA
        case 5 => registerB
        case 6 => registerC
        case x => x
      }

    def eval( instr: Instruction, operand: Int ): Machine = {
//      println( s"""A: $registerA | B: $registerB | C: $registerC
//                  |LOG: ${log.mkString_( "," )}
//                  |>> $instr $operand
//                  |""".stripMargin )

      instr match {
        case Instruction.adv =>
          copy( registerA = registerA >> combo( operand ).toInt, instrPointer = instrPointer + 2 )
        case Instruction.bxl =>
          copy( registerB = registerB ^ operand, instrPointer = instrPointer + 2 )
        case Instruction.bst =>
          copy( registerB = combo( operand ) % 8, instrPointer = instrPointer + 2 )
        case Instruction.jnz =>
          copy( instrPointer = if (loop && registerA != 0) operand else instrPointer + 2 )
        case Instruction.bxc =>
          copy( registerB = registerB ^ registerC, instrPointer = instrPointer + 2 )
        case Instruction.out =>
          copy( instrPointer = instrPointer + 2, log = log.append( combo( operand ) % 8 ) )
        case Instruction.bdv =>
          copy( registerB = registerA >> combo( operand ).toInt, instrPointer = instrPointer + 2 )
        case Instruction.cdv =>
          copy( registerC = registerA >> combo( operand ).toInt, instrPointer = instrPointer + 2 )
      }
    }

    def step: Option[Machine] =
      (
        program.lift( instrPointer ),
        program.lift( instrPointer + 1 )
      ).mapN( ( i, o ) => eval( Instruction( i ), o ) )

    @tailrec
    final def runAll: Machine = {
      step match {
        case Some( value ) => value.runAll
        case None          => this
      }
    }

    def output: Chain[BigInt] = runAll.log

  }

  sealed trait Instruction extends EnumEntry
  object Instruction extends Enum[Instruction] {
    def apply( code: Int ): Instruction = values( code )

    case object adv extends Instruction
    case object bxl extends Instruction
    case object bst extends Instruction
    case object jnz extends Instruction
    case object bxc extends Instruction
    case object out extends Instruction
    case object bdv extends Instruction
    case object cdv extends Instruction

    override val values: Vector[Instruction] = findValues.toVector
  }

  object parsers {
    def endl: Parser[Unit] = Rfc5234.lf | ( Rfc5234.cr ~ Rfc5234.lf.rep0 ).void

    val int: Parser[Int] = Numbers.nonNegativeIntString.mapFilter( _.toIntOption )

    def register( c: Char ): Parser[Int] = {
      Parser.string( "Register " ).void *> Parser.char( c ) *> Parser.char( ':' ) *> Rfc5234.wsp.rep.void *>
        int
    }

    val program: Parser[Vector[Int]] =
      Parser.string( "Program:" ).void *> Rfc5234.wsp.rep.void *>
        int.repSep0( Parser.char( ',' ) ).map( _.toVector )

    val machine: Parser[Machine] =
      (
        register( 'A' ) <* endl,
        register( 'B' ) <* endl,
        register( 'C' ) <* endl,
        endl,
        program <* endl.rep0
      ).mapN( ( a, b, c, _, p ) => Machine( a, b, c, 0, p, Chain.empty ) )
  }

  def parseMachine( input: Input ): Either[String, Machine] = {
    parsers.machine
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )
  }

  override def run( input: Input ): Either[String, String] =
    parseMachine( input )
      .map( _.output )
      .map( _.mkString_( "," ) )

  // Program: 2,4,1,1,7,5,0,3,4,7,1,6,5,5,3,0
  //   bst 4 // b <- a % 8
  //   bxl 1 // b <- b ^ 1
  //   cdv 5 // c <- a >> b
  //   adv 3 // a <- a >> 3
  //   bxc   // b <- b ^ c
  //   bxl 6 // b <- b ^ 6
  //   out 5 // output b
  //   jnz 0 // if a != 0 loop

  // a <- a >> 3
  // output (a % 8) ^ 7 ^ (a >> ((a % 8) ^ 1)) % 8

  case class Rule(
      a0: BigInt, // A & 7
      as: BigInt, // A >> s & 7
      s: Int,     // A & 7 ^ 1 (shift)
      result: BigInt
  ) {
    def value: BigInt = a0 | as << s
    def mask: BigInt  = 7 | 7 << s

    override def toString: String = s"$result if a0==$a0 && a$s==$as"
  }

  lazy val rules: Vector[Rule] = {
    def overlap( s: Int, a0: Int, a1: Int ): Boolean =
      ( a1 & ( ( 1 << ( 3 - s ).max( 0 ) ) - 1 ) ) == a0 >> s

    ( for {
      a0 <- 0 to 7
      s = a0 ^ 1
      a1 <- 0 to 7 if overlap( s, a0, a1 )
    } yield Rule( a0, a1, s, a0 ^ a1 ^ 7 ) ).toVector
  }

  case class Constraints( low: List[BigInt], highMask: BigInt, high: BigInt )

  def valid( out: BigInt, prev: Constraints ): Vector[Constraints] =
    rules
      .filter {
        case rule @ Rule( _, _, _, result ) =>
          result == out && ( ( rule.value & prev.highMask ) == ( prev.high & rule.mask ) )
      }
      .map {
        case Rule( a0, as, s, _ ) =>
          val newMask: BigInt = ( prev.highMask | 7 << s ) >> 3
          val newHigh: BigInt = ( prev.high | as << s ) >> 3 & newMask

          Constraints(
            a0 :: prev.low,
            newMask,
            newHigh
          )
      }

  def search( output: List[BigInt] ): Vector[BigInt] = {
    def toBigInt( octDigits: List[BigInt] ): BigInt =
      octDigits.foldLeft( BigInt( 0 ) )( ( acc, d ) => acc << 3 | d )

    @tailrec
    def go( solutions: Vector[BigInt], next: Vector[( List[BigInt], Constraints )] ): Vector[BigInt] =
      next.headOption match {
        case None => solutions
        case Some( ( remOut, constr ) ) =>
          remOut match {
            case Nil =>
              val solution: Option[BigInt] =
                Option.when( constr.high == 0 && constr.low.headOption.exists( _ != 0 ) )( toBigInt( constr.low ) )
              go( solutions ++ solution, next.drop( 1 ) )
            case out :: moreOut =>
              val newNext: Vector[( List[BigInt], Constraints )] = valid( out, constr ).tupleLeft( moreOut )
              go( solutions, next.drop( 1 ) ++ newNext )
          }
      }

    go( Vector.empty, Vector( ( output, Constraints( Nil, 0, 0 ) ) ) )
  }

  def findProgramAsOutput( machine: Machine ): Option[BigInt] = {
    val expectedOutputs: List[BigInt] = machine.program.map( BigInt( _ ) ).toList

    val solutions: Vector[BigInt] = search( expectedOutputs )

    solutions.minimumOption
  }

  override def runBonus( input: Input ): Either[String, String] =
    parseMachine( input )
      .flatMap( findProgramAsOutput( _ ).toRight( "Not found" ) )
      .map( _.toString )
}
