package aoc24

import cats.Group
import cats.Monoid
import cats.Order
import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Aoc21 extends Puzzle[Either[String, *]]( 21 ) {

  import Order.catsKernelOrderingForOrder

  case class V2( x: Int, y: Int ) {
    override def toString: String = s"($x, $y)"
  }

  object V2 {
    implicit val v2Group: Group[V2] = new Group[V2] {
      override def inverse( p: V2 ): V2        = V2( -p.x, -p.y )
      override def empty: V2                   = V2( 0, 0 )
      override def combine( p: V2, q: V2 ): V2 = V2( p.x + q.x, p.y + q.y )
    }

    implicit val v2Show: Show[V2]         = Show.fromToString
    implicit val v2Order: Order[V2]       = semiauto.order[V2]
    implicit val v2Ordering: Ordering[V2] = Order.catsKernelOrderingForOrder
  }

  sealed abstract class Direction( val v: V2, val repr: Char ) extends EnumEntry {
    lazy val next: Direction = Direction.values( (Direction.values.indexOf( this ) + 1) % Direction.values.size )
    lazy val prev: Direction = Direction.values( (Direction.values.indexOf( this ) + 3) % Direction.values.size )
  }

  object Direction extends Enum[Direction] {
    case object Up    extends Direction( V2( 0, -1 ), '^' )
    case object Right extends Direction( V2( 1, 0 ), '>' )
    case object Down  extends Direction( V2( 0, 1 ), 'v' )
    case object Left  extends Direction( V2( -1, 0 ), '<' )

    override val values: Vector[Direction]              = findValues.toVector
    implicit val directionOrder: Order[Direction]       = Order.by( _.v )
    implicit val directionOrdering: Ordering[Direction] = Order.catsKernelOrderingForOrder
  }

  sealed trait Key extends Product with Serializable { def face: Char }

  trait KeyEnum[K <: Key with EnumEntry] extends Enum[K] {
    implicit lazy val keyShow: Show[K]   = Show.show( _.face.toString )
    implicit lazy val keyOrder: Order[K] = Order.by( values.indexOf )

    lazy val byFace: Map[Char, K]          = values.toVector.fproductLeft( _.face ).toMap
    def getByFace( face: Char ): Option[K] = byFace.get( face )
    def keyA: K
  }

  sealed abstract class NumpadKey( override val face: Char ) extends EnumEntry with Key
  implicit object NumpadKey extends Enum[NumpadKey] with KeyEnum[NumpadKey] {
    case object Key0 extends NumpadKey( '0' )
    case object Key1 extends NumpadKey( '1' )
    case object Key2 extends NumpadKey( '2' )
    case object Key3 extends NumpadKey( '3' )
    case object Key4 extends NumpadKey( '4' )
    case object Key5 extends NumpadKey( '5' )
    case object Key6 extends NumpadKey( '6' )
    case object Key7 extends NumpadKey( '7' )
    case object Key8 extends NumpadKey( '8' )
    case object Key9 extends NumpadKey( '9' )
    case object KeyA extends NumpadKey( 'A' )

    override val values: Vector[NumpadKey] = findValues.toVector
    override def keyA: NumpadKey           = NumpadKey.KeyA
  }

  sealed abstract class DirpadKey( override val face: Char ) extends EnumEntry with Key
  implicit object DirpadKey extends Enum[DirpadKey] with KeyEnum[DirpadKey] {
    case object KeyUp    extends DirpadKey( '^' )
    case object KeyDown  extends DirpadKey( 'v' )
    case object KeyLeft  extends DirpadKey( '<' )
    case object KeyRight extends DirpadKey( '>' )
    case object KeyA     extends DirpadKey( 'A' )

    override val values: Vector[DirpadKey] = findValues.toVector
    override def keyA: DirpadKey           = DirpadKey.KeyA
  }

  case class Metric[C <: Key with EnumEntry: Order, R: Show: Order: Monoid]( m: Map[( C, C ), R] )(
      implicit E: KeyEnum[C]
  ) {
    override def toString: String =
      m.toVector.sorted
        .map { case ( ( from, to ), m ) => show"${s"${from.face} -> ${to.face}".padTo( 20, ' ' )}$m" }
        .mkString_( "\n" )

    def get( t: ( C, C ) ): Option[R] = m.get( t )

    def measure( goal: String ): Option[R] =
      goal.toVector
        .foldLeftM( ( E.keyA, Monoid.empty[R] ) ) {
          case ( ( prev, acc ), c ) =>
            E.getByFace( c )
              .flatMap( next => get( ( prev, next ) ).map( s => ( next, acc |+| s ) ) )
        }
        .map( _._2 )
  }

  case class StringMetric( self: String ) extends AnyVal {
    def length: Int = self.length
  }
  object StringMetric {
    implicit val stringMetricShow: Show[StringMetric]     = Show.show( s => s""""${s.self}"""" )
    implicit val stringMetricMonoid: Monoid[StringMetric] = semiauto.monoid[StringMetric]
    implicit val stringMetricOrder: Order[StringMetric]   = Order.by( sm => ( sm.length, sm.self ) )
  }

  case class Keypad[C <: Key with EnumEntry: Order]( keys: Map[C, V2], positions: Set[V2] )( implicit E: KeyEnum[C] ) {

    /*
     * 1. The "distance" bewteen two buttons on the dir-pad is always 1 for the human operator
     * 2. With (1) as a metric, find the length of the shortest sequence to move between any two buttons on the dir-pad.
     *    That's the metric for the first robot
     * 3. With (2) as a metric, repeat step 2
     *    That's the metric for the second robot
     * 4. With (3) as a metric, repeat step 3 but for the num-pad
     *    That's the metric for the third and final robot
     */

    def neighbours( p: V2 ): Vector[( V2, DirpadKey )] =
      Direction.values.mapFilter { d =>
        val q: V2                = p |+| d.v
        def k: Option[DirpadKey] = DirpadKey.getByFace( d.repr )
        Option.when( positions( q ) )( q ).flatMap( k.tupleLeft )
      }

    def metric[R: Monoid: Show: Order](
        operatorMetric: Metric[DirpadKey, R],
        debug: Boolean = false
    ): Either[String, Metric[C, R]] = {

      @tailrec
      def go(
          goal: V2,
          // NOTE states traversed. include the position and the last dirpad input used to reach it.
          //   i.e. the minimum info that determines the following operator actions
          seen: Set[( V2, DirpadKey )],
          open: SortedSet[( R, V2, DirpadKey, Boolean )]
      ): Option[R] =
        open.headOption match {
          case None => none // failed, should not happen
          case Some( ( dist, pos, opKey, done ) ) =>
            if (done)
              dist.some
            else if (seen( ( pos, opKey ) ))
              go( goal, seen, open.tail )
            else {
              val nextSeen: Set[( V2, DirpadKey )] = seen.incl( ( pos, opKey ) )
              val newOpen: Iterable[( R, V2, DirpadKey, Boolean )] =
                if (pos == goal)
                  operatorMetric
                    .get( ( opKey, DirpadKey.KeyA ) )
                    .map( c => ( dist |+| c, pos, DirpadKey.KeyA, true ) )
                else
                  neighbours( pos ).mapFilter {
                    case ( q, k ) =>
                      operatorMetric.get( ( opKey, k ) ).map( c => ( dist |+| c, q, k, false ) )
                  }
              go( goal, nextSeen, open.tail ++ newOpen )
            }
        }

      ( E.values.toVector, E.values.toVector ).tupled
        .traverse {
          case ( from, to ) =>
            go(
              keys( to ),
              Set.empty,
              SortedSet( ( Monoid.empty, keys( from ), DirpadKey.KeyA: DirpadKey, false ) )
            ).tupleLeft( ( from, to ) )
        }
        .map( v => Metric[C, R]( v.toMap ) )
        .toRight( s"Failed to compute next metric from\n$operatorMetric" )
    }

    lazy val humanTypingMetric: Metric[C, StringMetric] =
      Metric(
        ( E.values.toVector, E.values.toVector ).tupled.map {
          case ( f, c ) => ( ( f, c ), StringMetric( c.face.toString ) )
        }.toMap
      )

    lazy val humanCostMetric: Metric[C, Long] =
      Metric(
        ( E.values.toVector, E.values.toVector ).tupled.map { case ( f, c ) => ( ( f, c ), 1L ) }.toMap
      )

    def interpret( seq: String, start: C = E.keyA ): ( C, String, String ) = {
      val rev: Map[V2, C]                  = keys.map( _.swap )
      val dirsByRepr: Map[Char, Direction] = Direction.values.map( d => ( d.repr, d ) ).toMap
      seq.toList.foldLeft( ( start, "", "" ) ) {
        case ( ( pos, acc, op ), in ) =>
          val d: Option[Direction] = dirsByRepr.get( in )
          val out: Option[Char]    = Option.when( d.isEmpty )( pos.face )
          (
            rev( keys( pos ) |+| d.map( _.v ).orEmpty ),
            acc ++ out.mkString,
            s"$op$in${out.map( o => s"[$o]${Option.when( o == 'A' )( " " ).orEmpty}" ).orEmpty}"
          )
      }
    }

  }

  object Keypad {

    def of[C <: EnumEntry with Key: Order]( m: Map[Char, V2] )( implicit E: KeyEnum[C] ): Keypad[C] =
      E.values.toVector
        .traverse( k => m.get( k.face ).tupleLeft( k ).toValidNec( k.face ) )
        .fold(
          missing => throw new IllegalArgumentException( missing.mkString_( "missing faces: ", ", ", "" ) ),
          v => Keypad( v.toMap, v.map( _._2 ).toSet )
        )

    val numpad: Keypad[NumpadKey] = Keypad.of(
      Map(
        'A' -> V2( 2, 3 ),
        '0' -> V2( 1, 3 ),
        '1' -> V2( 0, 2 ),
        '2' -> V2( 1, 2 ),
        '3' -> V2( 2, 2 ),
        '4' -> V2( 0, 1 ),
        '5' -> V2( 1, 1 ),
        '6' -> V2( 2, 1 ),
        '7' -> V2( 0, 0 ),
        '8' -> V2( 1, 0 ),
        '9' -> V2( 2, 0 )
      )
    )

    val dirpad: Keypad[DirpadKey] = Keypad.of(
      Map(
        '<' -> V2( 0, 1 ),
        'v' -> V2( 1, 1 ),
        '>' -> V2( 2, 1 ),
        '^' -> V2( 1, 0 ),
        'A' -> V2( 2, 0 )
      )
    )
  }

  case class Code( src: String, value: Int )
  object Code {
    private val validChars = "0123456789A".toSet
    def apply( src: String ): Option[Code] =
      Option
        .when( src.forall( validChars ) )( () )
        .as(
          Code(
            src,
            src.filterNot( _ == 'A' ).dropWhile( _ == '0' ).toIntOption.getOrElse( 0 )
          )
        )

  }

  def parseCodes( input: Input ): Either[String, Vector[Code]] =
    input.lines
      .traverse( l => Code( l ).toValidNec( l ) )
      .leftMap( nec => nec.mkString_( "Invalid codes: ", ", ", "" ) )
      .toEither

  def costMetricWithDirpadRobots( robotCount: Int ): Either[String, Metric[NumpadKey, Long]] =
    ( Keypad.dirpad.humanCostMetric, robotCount ).tailRecM {
      case ( m, k ) =>
        if (k == 0) Keypad.numpad.metric( m ).map( Either.right )
        else Keypad.dirpad.metric( m ).map( nm => Either.left( ( nm, k - 1 ) ) )
    }

  def runWith( dirpadRobotCount: Int, input: Input ): Either[String, String] =
    for {
      codes  <- parseCodes( input )
      metric <- costMetricWithDirpadRobots( dirpadRobotCount )
      result <- codes.foldMapM(
                 code => metric.measure( code.src ).map( _ * code.value ).toRight( s"Cannot measure ${code.src}" )
               )
    } yield result.toString

  override def run( input: Input ): Either[String, String] = runWith( 2, input )

  override def runBonus( input: Input ): Either[String, String] = runWith( 25, input )
}
