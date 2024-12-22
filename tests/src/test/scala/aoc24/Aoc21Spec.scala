package aoc24

import cats.syntax.all._
import org.scalatest.matchers.must.Matchers
import scala.collection.immutable.SortedSet

class Aoc21Spec extends PuzzleSpec( Aoc21, "126384", "" ) with Matchers {
  import Aoc21._
  import cats.Order.catsKernelOrderingForOrder

  "the first order metric" should {
    "look like this" in {
      val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
        Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

      inside( firstOrderMetric ) {
        case Right( metric ) =>
          println( metric )
      }
    }
  }

  "the second order metric" should {
    "look like this" in {
      val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
        Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

      val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
        firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

      inside( secondOrderMetric ) {
        case Right( metric ) =>
          println( metric )
      }
    }
  }

  "the third order metric" should {
    "look like this" in {
      val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
        Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

      val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
        firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

      val thirdOrderMetric: Either[String, Metric[NumpadKey, StringMetric]] =
        secondOrderMetric.flatMap( Keypad.numpad.metric( _, debug = true ) )

      inside( thirdOrderMetric ) {
        case Right( metric ) =>
          println( metric )
      }
    }
  }

  "the Order[StringMetric]" should {
    "sort as expected" in {

      val s = SortedSet[StringMetric](
        StringMetric( "<v<A>>^A<vA<A>>^AAvA<^A>A" ),
        StringMetric( "<v<A>>^A<vA<A>>^AAvAA^A" )
      )

      s.toVector.map( _.self ) must ===( Vector( "<v<A>>^A<vA<A>>^AAvAA^A", "<v<A>>^A<vA<A>>^AAvA<^A>A" ) )

    }

    "sort tuples as expected" in {
      val s = SortedSet[( StringMetric, V2, DirpadKey, Boolean )](
        ( StringMetric( "<v<A>>^A<vA<A>>^AAvA<^A>A" ), V2( 0, 0 ), DirpadKey.KeyLeft, false ),
        ( StringMetric( "<v<A>>^A<vA<A>>^AAvAA^A" ), V2( 1, 2 ), DirpadKey.KeyDown, false )
      )

      s.toVector.map( _._1.self ) must ===( Vector( "<v<A>>^A<vA<A>>^AAvAA^A", "<v<A>>^A<vA<A>>^AAvA<^A>A" ) )
    }
  }

  def showInterpretation( seq: String, numpadStart: NumpadKey = NumpadKey.KeyA ): Unit = {
    println( s"  => $seq" )
    val ( _, firstRobotPresses: String, humanExplained: String ) = Keypad.dirpad.interpret( seq )
    println( humanExplained )
    println( s"  => $firstRobotPresses" )
    val ( _, secondRobotPresses: String, firstRobotExplained: String ) =
      Keypad.dirpad.interpret( firstRobotPresses )
    println( firstRobotExplained )
    println( s"  => $secondRobotPresses" )
    val ( _, thirdRobotPresses: String, secondRobotExplained: String ) =
      Keypad.numpad.interpret( secondRobotPresses, numpadStart )
    println( secondRobotExplained )
    println( thirdRobotPresses )
  }

  "the interpretations of the 379A sequence" should {

    "look like this" when {
      "interpreting the sample solution" in {
        val seq: String = "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"

        showInterpretation( seq )
      }

      "interpreting our solution" in {
        val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

        val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

        val thirdOrderMetric: Either[String, Metric[NumpadKey, StringMetric]] =
          secondOrderMetric.flatMap( Keypad.numpad.metric( _ ) )

        val seq: String = inside( thirdOrderMetric ) {
          case Right( metric ) =>
            inside( metric.measure( "379A" ) ) {
              case Some( input ) => input
            }
        }.self

        showInterpretation( seq )
      }
    }
  }

  "investigating the inputs of the diverging second robot sequences" when {
    val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
      Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

    val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
      firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

    def interpret( out: String ): Unit =
      inside( secondOrderMetric ) {
        case Right( metric ) =>
          inside( metric.measure( out ) ) {
            case Some( seq ) => showInterpretation( seq.self, NumpadKey.Key3 )
          }
      }

    "comparing interpretations" should {

      "look like this" when {
        "interpreting our solution" in {
          interpret( "<<^^A" )
        }

        "interpreting the sample solution" in {
          interpret( "^^<<A" )
        }
      }
    }

    "inspecting the second-order metric" should {
      "look like this" in {
        val tuplesOfInterest: Vector[Vector[( DirpadKey, DirpadKey )]] =
          Vector(
            // transitions in sample
            Vector(
              ( DirpadKey.KeyA, DirpadKey.KeyLeft ),
              ( DirpadKey.KeyLeft, DirpadKey.KeyUp ),
              ( DirpadKey.KeyUp, DirpadKey.KeyA )
            ),
            // our transtions
            Vector(
              ( DirpadKey.KeyA, DirpadKey.KeyUp ),
              ( DirpadKey.KeyUp, DirpadKey.KeyLeft ),
              ( DirpadKey.KeyLeft, DirpadKey.KeyA )
            )
          )

        inside( secondOrderMetric ) {
          case Right( metric ) =>
            println( "THEIRS".padTo( 32, ' ' ) + "OURS" )
            println(
              tuplesOfInterest.transpose
                .map(
                  ts => ts.map( t => show"$t ${metric.get( t ).orEmpty}".padTo( 32, ' ' ) ).mkString
                )
                .mkString( "\n" )
            )
        }

      }
    }
  }

  "the interpretations of the 179A sequence" should {

    "look like this" when {
      "interpreting the sample solution" in {
        val seq: String = "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"

        showInterpretation( seq )
      }

      "interpreting our solution" in {
        val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

        val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

        val thirdOrderMetric: Either[String, Metric[NumpadKey, StringMetric]] =
          secondOrderMetric.flatMap( Keypad.numpad.metric( _ ) )

        val seq: String = inside( thirdOrderMetric ) {
          case Right( metric ) =>
            inside( metric.measure( "179A" ) ) {
              case Some( input ) => input
            }
        }.self

        showInterpretation( seq )
      }
    }
  }

  "the shortest sequence lengths for the sample input" should {
    import org.scalatest.prop.TableDrivenPropertyChecks._

    val table =
      Table(
        ( "code", "expected", "length" ),
        ( "029A", "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A", 68 ),
        ( "980A", "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A", 60 ),
        ( "179A", "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 68 ),
        ( "456A", "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A", 64 ),
        ( "379A", "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 64 )
      )

    "be as long as expected" when {
      "using the typed input string metric" in {

        val firstOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          Keypad.dirpad.metric( Keypad.dirpad.humanTypingMetric )

        val secondOrderMetric: Either[String, Metric[DirpadKey, StringMetric]] =
          firstOrderMetric.flatMap( Keypad.dirpad.metric( _ ) )

        val thirdOrderMetric: Either[String, Metric[NumpadKey, StringMetric]] =
          secondOrderMetric.flatMap( Keypad.numpad.metric( _ ) )

        inside( thirdOrderMetric ) {
          case Right( metric ) =>
            forAll( table ) { ( code, expected, _ ) =>
              val result: Option[StringMetric] = metric.measure( code )

              inside( result ) {
                case Some( l ) => l.length must ===( expected.length )
              }
            }
        }
      }

      "using the length metric" in {
        val firstOrderMetricC: Either[String, Metric[DirpadKey, Long]] =
          Keypad.dirpad.metric( Keypad.dirpad.humanCostMetric )

        val secondOrderMetricC: Either[String, Metric[DirpadKey, Long]] =
          firstOrderMetricC.flatMap( Keypad.dirpad.metric( _ ) )

        val thirdOrderMetricC: Either[String, Metric[NumpadKey, Long]] =
          secondOrderMetricC.flatMap( Keypad.numpad.metric( _ ) )

        inside( thirdOrderMetricC ) {
          case Right( metric ) =>
            forAll( table ) { ( code, _, expectedLength ) =>
              val result: Option[Long] = metric.measure( code )

              inside( result ) {
                case Some( l ) => l must ===( expectedLength.toLong )
              }
            }
        }

      }
    }
  }

}
