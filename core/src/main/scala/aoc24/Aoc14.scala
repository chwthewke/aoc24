package aoc24

import cats.Monoid
import cats.data.Kleisli
import cats.derived.semiauto
import cats.effect.IO
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import com.sksamuel.scrimage.nio.PngWriter
import fs2.Stream
import java.awt.image.BufferedImage
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import scala.concurrent.duration.Duration
import scodec.bits.ByteVector

object Aoc14 extends Puzzle[Kleisli[IO, IsSample, *]]( 14 ) {

  override def timeout: Duration = Duration.Inf

  case class V2( x: Int, y: Int ) {
    def *:( n: Int ): V2 = V2( n * x, n * y )
    def %( rhs: V2 ): V2 = V2( x % rhs.x, y % rhs.y )

    def quadrant( dim: V2 ): Option[Int] = {
      // which half? -1 if p < (s-1)/2, 1 if p > (s-1)/2, 0 if p == (s-1)/2
      def h( p: Int, s: Int ): Int = (2 * p + 1 - s).sign

      // just making sure to return 4 different ints for the quadrants
      ( h( x, dim.x ), h( y, dim.y ) ) match {
        case ( 0, _ ) | ( _, 0 ) => None
        case ( qx, qy )          => Some( (qx + 1) / 2 + (qy + 1) )
      }
    }

    override def toString: String = s"($x, $y)"
  }

  object V2 {
    implicit val v2Monoid: Monoid[V2] = semiauto.monoid
  }

  case class Robot( pos: V2, vel: V2 ) {
    def posAt( dt: Int, dim: V2 ): V2 = ((dt *: vel |+| pos) % dim |+| dim) % dim
  }

  case class SecurityRoom( dim: V2, robots: Vector[Robot] ) {
    def flip( pos: V2 ): V2 =
      V2( dim.x - 1 - pos.x, dim.y - 1 - pos.y )

    lazy val positions: Set[V2] = robots.iterator.map( _.pos ).toSet

    def securityRating( dt: Int ): Int = {
      val quadrantContents: Vector[Int] = robots
        .map( _.posAt( dt, dim ) )
        .mapFilter( p => p.quadrant( dim ) )
        .foldMap( q => Map( q -> 1 ) )
        .values
        .toVector

      if (quadrantContents.size == 4) quadrantContents.product
      else 0
    }

    def at( dt: Int ): SecurityRoom =
      SecurityRoom( dim, robots.map( r => r.copy( pos = r.posAt( dt, dim ) ) ) )

    def show: String = {
      val counts: Map[V2, Int] =
        robots.map( _.pos ).foldMap( p => Map( p -> 1 ) )

      (0 until dim.y)
        .map(
          j =>
            (0 until dim.x).map { i =>
              val p = V2( i, j )
              p.quadrant( dim ).as( counts.get( p ).fold( "." )( _.toString ) ).getOrElse( " " )
            }.mkString
        )
        .mkString( "\n" )
    }
  }

  object ImageGen {
    import com.sksamuel.scrimage._
    import java.awt.Color
    import scalatags.Text.all._
    import scalatags.Text.tags2

    def imageOf( room: SecurityRoom ): ImmutableImage =
      ImmutableImage
        .create( room.dim.x, room.dim.y, BufferedImage.TYPE_BYTE_BINARY )
        .map(
          pix =>
            if (room.positions.contains( V2( pix.x, pix.y ) ))
              Color.WHITE
            else
              Color.BLACK
        )

    def srcData( image: ImmutableImage ): String =
      s"data:image/png;base64,${ByteVector( image.forWriter( PngWriter.MinCompression ).bytes() ).toBase64}"

    def htmlImageOf( room: SecurityRoom, dt: Int ): Tag =
      figure(
        img( src := srcData( imageOf( room.at( dt ) ) ) ),
        figcaption( dt.toString )
      )

    def htmlPage( room: SecurityRoom, pn: Int ): IO[doctype] = {
      Stream
        .emits[IO, Int]( 0 until 100 )
        .map( _ + pn * 100 )
        .parEvalMap( 8 )( i => IO.blocking( div( htmlImageOf( room, i ) ) ) )
        .compile
        .toVector
        .map(
          images =>
            doctype( "HTML" )(
              html(
                head( tags2.title( s"AoC 14 - page $pn" ) ),
                body(
                  div(
                    display.flex,
                    a(
                      href := (pn - 1).toString,
                      Option.when( pn == 0 )( disabled ),
                      strong( "PREVIOUS" )
                    ),
                    div( flexGrow := "1" ),
                    a( href := (pn + 1).toString, strong( "NEXT" ) )
                  ),
                  div(
                    display.flex,
                    flexWrap.wrap,
                    images
                  )
                )
              )
            )
        )
    }
  }

  class HttpServer( val room: SecurityRoom ) extends Http4sDsl[IO] {

    import com.comcast.ip4s._
    import org.http4s.implicits._
    import org.http4s.scalatags._

    val routes: HttpRoutes[IO] = HttpRoutes.of {
      case GET -> Root / IntVar( pn ) => Ok( ImageGen.htmlPage( room, pn ) )
    }

    def run: IO[Nothing] =
      EmberServerBuilder
        .default[IO]
        .withHost( ipv4"127.0.0.1" )
        .withPort( port"8000" )
        .withHttpApp( routes.orNotFound )
        .build
        .useForever
  }

  def roomDimensions( isSample: IsSample ): V2 =
    if (isSample.value) V2( 11, 7 )
    else V2( 101, 103 )

  object parsers {
    val posInt: Parser[Int] = Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
    val int: Parser[Int]    = Numbers.bigInt.map( _.toInt )

    def coords( p: Parser[Int] ): Parser[V2] = ( p, Parser.char( ',' ), p ).mapN( ( x, _, y ) => V2( x, y ) )

    val robot: Parser[Robot] =
      (
        Parser.string( "p=" ) *> coords( posInt ),
        Rfc5234.wsp *> Parser.string( "v=" ) *> coords( int )
      ).mapN( Robot )
  }

  def parseRobots( input: Input, isSample: IsSample ): IO[SecurityRoom] =
    input.lines
      .traverse( parsers.robot.parseAll )
      .leftMap( err => new RuntimeException( s"Parser error $err" ) )
      .liftTo[IO]
      .map( SecurityRoom( roomDimensions( isSample ), _ ) )

  override def run( input: Input ): Kleisli[IO, IsSample, String] =
    Kleisli(
      isSample =>
        parseRobots( input, isSample )
          .map( _.securityRating( 100 ) )
          .map( _.toString )
    )

  override def runBonus( input: Input ): Kleisli[IO, IsSample, String] =
    Kleisli(
      isSample =>
        parseRobots( input, isSample )
          .flatMap( new HttpServer( _ ).run )
    )
}
