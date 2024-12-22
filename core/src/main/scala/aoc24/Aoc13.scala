package aoc24

import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.effect.IO
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._

object Aoc13 extends Puzzle[IO]( 13 ) {
  case class V2( x: Long, y: Long ) {
    override def toString: String = s"($x, $y)"
  }

  object V2 {
    implicit val v2Monoid: Monoid[V2] = semiauto.monoid[V2]
    val zero: V2                      = v2Monoid.empty
    implicit val v2Show: Show[V2]     = Show.fromToString
  }

  val clawOffset: Long = 10_000_000_000_000L

  case class ClawMachine(
      buttonA: V2,
      buttonB: V2,
      prize: V2
  ) {

    // u * a.x + v * b.x = p.x
    // u * a.y + v * b.y = p.y
    def moves: Option[( Long, Long )] = {
      // u * a.x * a.y + v * b.x * a.y = p.x * a.y (1)
      // u * a.x * a.y + v * b.y * a.x = p.y * a.x (2)

      // u * a.x * b.y + v * b.x * b.y = p.x * b.y (3)
      // u * a.y * b.x + v * b.y * b.x = p.y * b.x (4)

      val det: Long   = buttonA.x * buttonB.y - buttonA.y * buttonB.x
      val paDet: Long = buttonA.x * prize.y - buttonA.y * prize.x // (2) - (1): v * det = paDet
      val pbDet: Long = buttonB.y * prize.x - buttonB.x * prize.y // (3) - (4): u * det = pbDet

//        println(
//          s"""A = $buttonA B = $buttonB P = $prize
//             |
//             |det = ${buttonA.x} * ${buttonB.y} - ${buttonA.y} * ${buttonB.x} = $det
//             |paDet = ${buttonA.x} * ${prize.y} - ${buttonA.y} * ${prize.x} = $paDet${Option
//               .when( paDet % det == 0 )( s" = ${paDet / det} * det" )
//               .orEmpty}
//             |pbDet = ${prize.x} * ${buttonB.y} - ${prize.y} * ${buttonB.x} = $pbDet${Option
//               .when( pbDet % det == 0 )( s" = ${pbDet / det} * det" )
//               .orEmpty}
//             |
//             |""".stripMargin
//        )

      if (det != 0L) {
        Option.when( paDet % det == 0 && pbDet % det == 0 )( ( ( pbDet / det ), ( paDet / det ) ) )
      } else {
        None // TODO, but all actual solutions are determinate lol
      }
    }

    def cost: Long = moves.fold( 0L ) { case ( a, b ) => 3 * a + b }

    def withClawOffset: ClawMachine =
      copy( prize = V2( prize.x + clawOffset, prize.y + clawOffset ) )
  }

  object parsers {
    def coord( c: Char, s: Char ): Parser[Long] =
      Parser.char( c ) *> Parser.char( s ) *> Numbers.nonNegativeIntString.mapFilter( _.toLongOption )

    def button( c: Char ): Parser[V2] =
      Parser.string( "Button" ) *> Rfc5234.wsp.rep *> Parser.char( c ) *> Parser.char( ':' ) *> Rfc5234.wsp.rep *>
        (
          coord( 'X', '+' ),
          Parser.char( ',' ) *> Rfc5234.wsp.rep,
          coord( 'Y', '+' )
        ).mapN( ( x, _, y ) => V2( x, y ) )

    val prize: Parser[V2] =
      Parser.string( "Prize:" ) *> Rfc5234.wsp.rep *>
        (
          coord( 'X', '=' ),
          Parser.char( ',' ) *> Rfc5234.wsp.rep,
          coord( 'Y', '=' )
        ).mapN( ( x, _, y ) => V2( x, y ) )

    val endl: Parser[Unit] = Rfc5234.lf | Rfc5234.cr | Rfc5234.crlf

    val machine: Parser[ClawMachine] =
      (
        button( 'A' ) <* endl,
        button( 'B' ) <* endl,
        prize
      ).mapN( ClawMachine )

    val machines: Parser[Vector[ClawMachine]] =
      machine.repSep( endl.rep ).map( _.toNev.toVector )
  }

  def parseMachines( input: Input ): IO[Vector[ClawMachine]] =
    parsers.machines
      .parseAll( input.trimmed )
      .leftMap( err => new RuntimeException( s"Parser error $err" ) )
      .liftTo[IO]

  override def run( input: Input ): IO[String] =
    parseMachines( input )
      .map( _.foldMap( _.cost ) )
      .map( _.toString )

  override def runBonus( input: Input ): IO[String] =
    parseMachines( input )
      .map( _.map( _.withClawOffset ) )
      .map( _.foldMap( _.cost ) )
      .map( _.toString )

}
