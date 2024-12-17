package aoc24

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Aoc17Spec extends PuzzleSpec( Aoc17, "4,6,3,5,6,3,5,2,1,0", "" ) {

  implicit def noShrinkGenerators[T]: Shrink[T] = Shrink.shrinkAny

  import Aoc17._

  "The machine parser" should {
    behave like parseTheRawSampleAs( Aoc17.parsers.machine ) {
      case Machine( registerA, registerB, registerC, _, program, _, _ ) =>
        registerA must ===( 729: BigInt )
        registerB must ===( 0: BigInt )
        registerC must ===( 0: BigInt )

        program must ===( Vector(0, 1, 5, 4, 3, 0 ) )
    }
  }

  "The output rules" should {
    "be shown" in {
      rules
        .sortBy { case Rule( a0, a1, s, _ ) => ( a0, s, a1 ) }
        .foreach( println )
    }
  }

  val template: Machine =
    loadRealInput()
      .flatMap( parseMachine( _ ).leftMap( new RuntimeException( _ ) ).liftTo[IO] )
      .unsafeRunSync()

  "The no-loop output" should {

    "match the rule-based prediction" in {
      import ScalaCheckDrivenPropertyChecks._

      implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration( minSuccessful = 100 )

      val rules: Vector[Rule] = Aoc17.rules

      forAll( Gen.choose[BigInt]( 0, 1023 ) ) { n =>
        // table-based
        val machine: Machine = template.copy( registerA = n, loop = false )

        val ruleBased: Option[BigInt] =
          rules.collectFirst { case Rule( a0, a1, s, r ) if (n & 7) == a0 && ((n >> s) & 7) == a1 => r }

        // actual
        val output: Option[BigInt] = machine.output.headOption

        inside( ( ruleBased, output ) ) {
          case ( Some( exp ), Some( act ) ) => act must ===( exp )
        }

      }
    }
  }

  "The search-for-quine function" should {
    "produce register values that, when run, output the desired sequence" when {
      import TableDrivenPropertyChecks._

      def verifySolutions( length: Int ): Unit =
        s"the sequence has length $length" in {
          val outputSeqs: Vector[List[BigInt]] = (BigInt( 0 ) to 7).toVector.replicateA( length )
          val table                            = Table( "outputs", outputSeqs: _* )

          forAll( table ) { out =>
            val solutions: Vector[BigInt] = search( out )

            println(
              solutions.mkString( s"${solutions.size} solutions for out = [${out.mkString( ", " )}]\n  ", "\n  ", "" )
            )

            solutions.foreach { n =>
              val result: Chain[BigInt] = template.copy( registerA = n ).output

              result.toList must ===( out )
            }
          }
        }

      behave like verifySolutions( 1 )
      behave like verifySolutions( 2 )
      behave like verifySolutions( 3 )

    }
  }

}
