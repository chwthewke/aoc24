package aoc24

import cats.Eval
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.Path
import org.scalameta.ascii.graph.Graph
import org.scalameta.ascii.layout.GraphLayout
import org.scalameta.ascii.layout.prefs.LayoutPrefsImpl
import org.scalatest.matchers.must.Matchers
import scala.collection.immutable.SortedSet

class Aoc24Spec extends PuzzleSpec( Aoc24, "2024", "" ) with Matchers {
  import Aoc24._

  "The circuit parser" should {
    parseTheRawSample( Aoc24.parsers.circuit )
  }

  val circuit: Circuit =
    loadRealInput()
      .flatMap( parseCircuit( _ ).leftMap( err => new RuntimeException( err ) ).liftTo[IO] )
      .unsafeRunSync()

  "The dependencies between inputs and outputs" should {
    "be as follows" in {
      println(
        circuit.dependencies.toVector
          .sortBy( _._1 )
          .mapFilter {
            case ( gate, dependencies ) =>
              Gate.Output.asZ( gate ).as {
                val inputs: SortedSet[Gate] =
                  dependencies.filter( g => Gate.Input.asX( g ).orElse( Gate.Input.asY( g ) ).isDefined )
                show"$gate <- ${inputs.mkString_( ", " )}"
              }
          }
          .mkString( "\n" )
      )
    }
  }

  "the ASCII graph of the circuit" should {
    "look like this" ignore {
      val vertices =
        ( circuit.inputs.keySet ++
          circuit.connections.map( _._4 ).sorted ).map( _.name )

      val edges = circuit.connections
        .flatMap {
          case ( in1, in2, _, out ) => Vector( ( out.name, in1.name ), ( out.name, in2.name ) )
        }

      val graph: Graph[String] = Graph( vertices, edges.toList )

      fs2.io.file
        .Files[IO]
        .writeAll( Path( "graph.txt" ) )(
          Stream
            .emit[IO, String](
              GraphLayout.renderGraph( graph, layoutPrefs = LayoutPrefsImpl( elevateEdges = false ) )
            )
            .through( fs2.text.utf8.encode )
        )
        .compile
        .drain
        .unsafeRunSync()

    }
  }

  "the XOR of all matching input bits" should {
    "be in the circuit" in {
      val expected: Vector[( Gate, Gate, Operator )] =
        0.to( 44 ).map( n => ( Gate( f"x$n%02d" ), Gate( f"y$n%02d" ), Operator.Xor ) ).toVector

      val actual: Vector[( Gate, Gate, Operator )] =
        circuit.connections.map { case ( in1, in2, op, _ ) => ( in1 min in2, in1 max in2, op ) }

      expected.foreach( actual must contain( _ ) )
      actual must contain allElementsOf expected
    }
  }

  "the AND of all matching input bits" should {
    "be in the circuit" in {
      val expected: Vector[( Gate, Gate, Operator )] =
        0.to( 44 ).map( n => ( Gate( f"x$n%02d" ), Gate( f"y$n%02d" ), Operator.And ) ).toVector

      val actual: Vector[( Gate, Gate, Operator )] =
        circuit.connections.map { case ( in1, in2, op, _ ) => ( in1 min in2, in1 max in2, op ) }

      expected.foreach( actual must contain( _ ) )
      actual must contain allElementsOf expected
    }
  }

  "Some of the first bits of the adder" should {
    "compute the output and carry" in {
      def stop[A]: Eval[Either[A, Unit]] = Eval.always( Right( println( "stop" ) ) )

      val swaps = Vector(
        ( Gate( "vcv" ), Gate( "z13" ) ),
        ( Gate( "vwp" ), Gate( "z19" ) ),
        ( Gate( "mps" ), Gate( "z25" ) ),
        ( Gate( "vjv" ), Gate( "cqm" ) )
      )

      inside( circuit.withSwaps( swaps ) ) {
        case Some( mCircuit ) =>
          ( mCircuit.firstAdderBit, 1 )
            .tailRecM[Eval, Unit] {
              case ( None, _ ) => stop
              case ( Some( ( _, carry ) ), i ) =>
                Eval.later( Left( ( mCircuit.buildAdderBit( i, carry ), i + 1 ) ) )
            }
            .value
      }

      println( swaps.flatMap { case ( g, h ) => Vector( g, h ) }.sorted.mkString_( "," ) )
    }
  }

  "The autonomous repair procedure" should {
    "complete some levels" in {
      computeLevels( circuit )
    }
  }

}
