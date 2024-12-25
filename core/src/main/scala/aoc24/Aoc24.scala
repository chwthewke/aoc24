package aoc24

import cats.Order
import cats.Show
import cats.data.OptionT
import cats.data.State
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all._
import cats.~>
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Aoc24 extends Puzzle[Either[String, *]]( 24 ) {
  case class Gate( name: String ) extends AnyVal
  object Gate {
    object Output {
      def z( i: Int ): Gate              = Gate( f"z$i%02d" )
      def asZ( gate: Gate ): Option[Int] = parsers.outputGate.parseAll( gate.name ).toOption
    }

    object Input {
      def x( i: Int ): Gate = Gate( f"x$i%02d" )
      def y( i: Int ): Gate = Gate( f"y$i%02d" )

      def asX( gate: Gate ): Option[Int] = parsers.numberGate( 'x' ).parseAll( gate.name ).toOption
      def asY( gate: Gate ): Option[Int] = parsers.numberGate( 'y' ).parseAll( gate.name ).toOption
    }

    implicit val gateShow: Show[Gate]         = Show.show( _.name )
    implicit val gateOrder: Order[Gate]       = Order.by( _.name )
    implicit val gateOrdering: Ordering[Gate] = Order.catsKernelOrderingForOrder
  }

  sealed abstract class Operator( val repr: String, val apply: ( Boolean, Boolean ) => Boolean ) extends EnumEntry {
    override def toString: String = repr
  }
  object Operator extends Enum[Operator] {
    case object And extends Operator( "AND", ( x, y ) => x && y )
    case object Or  extends Operator( "OR", ( x, y ) => x || y )
    case object Xor extends Operator( "XOR", ( x, y ) => x ^ y )

    override val values: Vector[Operator] = findValues.toVector

    implicit val operatorShow: Show[Operator] = Show.fromToString
  }

  type Connection = ( Gate, Gate, Operator, Gate )
  implicit val connectionShow: Show[Connection] = Show.show {
    case ( in1, in2, op, out ) => show"$in1 $op $in2 -> $out"
  }

  case class Connections( connections: Vector[Connection] ) {
    def findInputXor( i: Int ): Option[Gate] =
      findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.Xor )

    def findInputAnd( i: Int ): Option[Gate] =
      findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.And )

    def findO( lhs: Gate, rhs: Gate, operator: Operator ): Option[Gate] =
      connections.collectFirstSome {
        case ( in1, in2, op, out ) =>
          Option.when( Set( in1, in2 ) == Set( lhs, rhs ) && op == operator )( out )
      }

    def findIO( input: Gate, operator: Operator ): Option[( Gate, Gate )] =
      connections.collectFirstSome {
        case ( in1, in2, op, out ) =>
          Option
            .when( op == operator )(
              Option.when( in1 == input )( ( in2, out ) ).orElse( Option.when( in2 == input )( ( in1, out ) ) )
            )
            .flatten
      }

    def evaluableWith( inputs: Set[Gate] ): Vector[Connection] = {
      @tailrec
      def go(
          evaluated: Set[Gate],
          evaluable: Vector[Connection],
          toEvaluate: Vector[Connection]
      ): Vector[Connection] = {
        val ( moreEvaluable, notEvaluable ) =
          toEvaluate.partition { case ( in1, in2, _, _ ) => Vector( in1, in2 ).forall( evaluated ) }
        if (moreEvaluable.isEmpty) evaluable
        else go( evaluated ++ moreEvaluable.map( _._4 ), evaluable ++ moreEvaluable, notEvaluable )
      }

      go( inputs, Vector.empty, connections )
    }
  }

  case class Circuit(
      connectionsList: Connections,
      inputs: Map[Gate, Boolean]
  ) {

    def connections: Vector[( Gate, Gate, Operator, Gate )] = connectionsList.connections

    def finish( values: Map[Gate, Boolean] ): BigInt =
      values.toVector.mapFilter {
        case ( gate, value ) =>
          Gate.Output.asZ( gate ).flatMap( i => Option.when( value )( BigInt( 1 ) << i ) )
      }.sum

    def evaluate: BigInt =
      finish( connectionsList.connections.foldLeft( inputs ) {
        case ( values, ( in1, in2, op, out ) ) =>
          values.updated( out, op.apply( values( in1 ), values( in2 ) ) )
      } )

    def dependencies: Map[Gate, SortedSet[Gate]] =
      connectionsList.connections.foldLeft( inputs.map { case ( g, _ ) => ( g, SortedSet( g ) ) } ) {
        case ( acc, ( in1, in2, _, out ) ) =>
          acc.updated( out, acc( in1 ) ++ acc( in2 ) )
      }

    def firstAdderBit: Option[( Gate, Gate )] =
      ( connectionsList.findInputXor( 0 ), connectionsList.findInputAnd( 0 ) ).tupled

    def buildAdderBit(
        i: Int,
        carry: Gate
    ): Option[( Gate, Gate )] = {
      val and: Option[Gate] = connectionsList.findInputAnd( i )
      val xor: Option[Gate] = connectionsList.findInputXor( i )

      println( show"""===============
                     |i=$i AND=$and XOR=$xor CARRY_${i - 1}=$carry""".stripMargin )

      val evaluable: Connections =
        Connections( connectionsList.evaluableWith( Set( carry ) ++ and ++ xor ) )

      println( evaluable.connections.mkString_( s"Evaluable at step $i:\n  ", "\n  ", "" ) )

      val outputGate: Gate = Gate.Output.z( i )

      def findOutputBit: Option[( Vector[( Gate, Gate )], Gate )] =
        xor
          .flatMap( evaluable.findO( _, carry, Operator.Xor ) )
          .map( gate =>
            if (gate == outputGate) ( Vector.empty, gate )
            else ( Vector( ( gate, outputGate ) ), outputGate )
          )

      val outputBit: Option[Gate] = findOutputBit.map( _._2 )

      println( show"output bit: $outputBit" )

      val carryStep1: Option[Gate] =
        xor.flatMap( evaluable.findO( _, carry, Operator.And ) )

      println( show"carry step 1: $carryStep1" )

      val carryStep2: Option[Gate] =
        ( carryStep1, and ).flatMapN( evaluable.findO( _, _, Operator.Or ) )

      println( show"carry step 2: $carryStep2" )
      ( outputBit, carryStep2 ).tupled
    }

    def withSwaps( swaps: Vector[( Gate, Gate )] ): Option[Circuit] =
      Circuit
        .of(
          inputs,
          connections.map {
            case conn @ ( in1, in2, op, out ) =>
              swaps
                .collectFirst {
                  case ( `out`, repl ) => ( in1, in2, op, repl )
                  case ( repl, `out` ) => ( in1, in2, op, repl )
                }
                .getOrElse( conn )
          }
        )
        .toOption

  }

  case class SearchState( swapsLog: Vector[Set[Gate]], circuit: Circuit, carries: List[Gate] ) {

    def swap( g: Gate, h: Gate ): Option[SearchState] = {

      // attempt merge with latest swap
      val newSwaps: Vector[Set[Gate]] =
        swapsLog.lastOption.fold( swapsLog :+ Set( g, h ) ) { s =>
          val next = Set( g, h )
          val d    = s.diff( next ).union( next.diff( s ) )
          d.toList match {
            case _ :: _ :: Nil => swapsLog.init :+ d
            case _             => swapsLog :+ next
          }
        }

      circuit
        .withSwaps( Vector( ( g, h ) ) )
        .map( newCircuit => copy( circuit = newCircuit, swapsLog = newSwaps ) )
    }

    def push( carry: Gate ): SearchState =
      copy( carries = carry :: carries )

    def validate: Option[String] = {
      val outputBits: Int = circuit.connections.map( _._4 ).count( out => Gate.Output.asZ( out ).isDefined )

      Option.when( swapsLog.size <= 4 && carries.size + 1 == outputBits )(
        swapsLog.flatMap( _.toVector ).sorted.mkString_( "," )
      )
    }

  }

  trait StateFunctions[F[_]] { self =>
    def pure[A]( a: A ): F[A]
    def get: F[SearchState]
    def set( state: SearchState ): F[Unit]
    def modify( f: SearchState => SearchState ): F[Unit]
    def inspect[A]( f: SearchState => A ): F[A]

    def mapK[G[_]]( t: F ~> G ): StateFunctions[G] = new StateFunctions[G] {
      override def pure[A]( a: A ): G[A]                            = t( self.pure( a ) )
      override def get: G[SearchState]                              = t( self.get )
      override def set( state: SearchState ): G[Unit]               = t( self.set( state ) )
      override def inspect[A]( f: SearchState => A ): G[A]          = t( self.inspect( f ) )
      override def modify( f: SearchState => SearchState ): G[Unit] = t( self.modify( f ) )
    }

  }

  object SearchState extends StateFunctions[State[SearchState, *]] {
    type P[x] = State[SearchState, x]
    type O[x] = OptionT[P, x]

    val option: StateFunctions[O]           = mapK( OptionT.liftK )
    def fromOption[A]( o: Option[A] ): O[A] = OptionT.fromOption( o )

    override def pure[A]( a: A ): P[A]                                             = State.pure( a )
    override def get: P[SearchState]                                               = State.get
    override def inspect[A]( f: SearchState => A ): State[SearchState, A]          = State.inspect( f )
    override def set( state: SearchState ): P[Unit]                                = State.set( state )
    override def modify( f: SearchState => SearchState ): State[SearchState, Unit] = State.modify( f )

    def push( g: Gate ): O[Unit] = option.modify( _.push( g ) )
    def swap( g: Gate, h: Gate ): O[Unit] = option.get.subflatMap( _.swap( g, h ) ).semiflatMap( set ) <*
      option.pure( println( show"!! $g <-> $h" ) )

    def findO( g: Gate, h: Gate, op: Operator ): O[Gate] =
      option.get.flatMap( state => fromOption( state.circuit.connectionsList.findO( g, h, op ) ) )
    def findIO( g: Gate, op: Operator ): O[( Gate, Gate )] =
      option.get.flatMap( state => fromOption( state.circuit.connectionsList.findIO( g, op ) ) )

    def init( circuit: Circuit ): SearchState = SearchState( Vector.empty, circuit, Nil )

    def computeFirstLevel: O[Unit] = for {
      outputBit <- findO( Gate.Input.x( 0 ), Gate.Input.y( 0 ), Operator.Xor )
      outputTarget = Gate.Output.z( 0 )
      _     <- swap( outputTarget, outputBit ).whenA( outputBit != outputTarget )
      carry <- findO( Gate.Input.x( 0 ), Gate.Input.y( 0 ), Operator.And )
      _     <- push( carry )
    } yield ()

    def logState: O[Unit] =
      option
        .inspect( s => ( s.carries.size, s.carries.headOption ) )
        .flatMap {
          case ( _, None ) => option.pure( () )
          case ( i, Some( c ) ) =>
            OptionT.liftF(
              (
                findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.Xor ).value,
                findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.And ).value
              ).mapN( ( xo, ao ) =>
                println( //
                  show"""==========================================================
                        |i=$i AND=${ao.fold( "-" )( _.show )} XOR=${xo.fold( "-" )( _.show )} CARRY_${i - 1}=$c""" //
                    .stripMargin
                )
              )
            )
        }

    def computeLevel: O[Unit] = for {
      _       <- logState
      carries <- option.inspect( _.carries )
      i            = carries.size
      outputTarget = Gate.Output.z( i )
      prevCarry                 <- fromOption( carries.headOption )
      xorCand                   <- findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.Xor )
      _                         <- option.pure( println( show">>> xor = $xorCand" ) )
      ( xor, outputBit )        <- findIO( prevCarry, Operator.Xor )
      _                         <- swap( xor, xorCand ).whenA( xor != xorCand )
      _                         <- option.pure( println( show">>> output <- $outputBit" ) )
      _                         <- swap( outputTarget, outputBit ).whenA( outputBit != outputTarget )
      and                       <- findO( Gate.Input.x( i ), Gate.Input.y( i ), Operator.And )
      _                         <- option.pure( println( show">>> and = $and" ) )
      preCarryBit               <- findO( xor, prevCarry, Operator.And )
      ( preCarryTarget, carry ) <- findIO( and, Operator.Or )
      _                         <- swap( preCarryBit, preCarryTarget ).whenA( preCarryTarget != preCarryBit )
      _                         <- push( carry )
    } yield ()

  }

  def computeLevels( circuit: Circuit ): SearchState =
    ( SearchState.computeFirstLevel *> SearchState.computeLevel.foreverM ).value
      .runS( SearchState.init( circuit ) )
      .value

  def computeLevelsAndValidate( circuit: Circuit ): Either[String, String] =
    computeLevels( circuit ).validate.toRight( "No luck" )

  object Circuit {
    def topoSort( inputs: Set[Gate], connections: Vector[Connection] ): Either[String, Vector[Connection]] = {

      @tailrec
      def go(
          seen: Set[Gate],
          acc: List[Connection],
          remaining: Vector[Connection]
      ): Either[String, Vector[Connection]] =
        if (remaining.isEmpty)
          Right( acc.toVector.reverse )
        else {
          val ix: Int = remaining.indexWhere { case ( in1, in2, _, _ ) => seen( in1 ) && seen( in2 ) }
          if (ix < 0)
            Either.left(
              s"Topological sort failed after evaluating ${seen.removedAll( inputs ).toVector.mkString_( "," )}"
            )
          else
            go( seen.incl( remaining( ix )._4 ), remaining( ix ) :: acc, remaining.patch( ix, Vector.empty, 1 ) )
        }

      go( inputs, Nil, connections )
    }

    def of( inputs: Map[Gate, Boolean], connections: Vector[Connection] ): Either[String, Circuit] =
      topoSort( inputs.keySet, connections ).map( sc => Circuit( Connections( sc ), inputs ) )
  }

  object parsers {
    def endl: Parser[Unit] = Rfc5234.lf | ( Rfc5234.cr ~ Rfc5234.lf.rep0 ).void

    val gate: Parser[Gate] =
      Parser.charWhere( c => ( c >= 'a' && c <= 'z' ) || ( c >= '0' && c <= '9' ) ).rep( 3 ).string.map( Gate( _ ) )

    val bit: Parser[Boolean] = Parser.charIn( "01" ).map( _ == '1' )

    val operator: Parser[Operator] =
      Operator.values.map( op => Parser.string( op.repr ).as( op ) ).foldK

    val input: Parser[( Gate, Boolean )] =
      ( gate <* Parser.char( ':' ) <* Rfc5234.wsp.rep, bit ).tupled

    val connection: Parser[Connection] =
      (
        gate <* Rfc5234.wsp.rep,
        operator <* Rfc5234.wsp.rep,
        gate <* Rfc5234.wsp.rep <* Parser.string( "->" ) <* Rfc5234.wsp.rep,
        gate
      ).mapN( ( in1, op, in2, out ) => ( in1, in2, op, out ) )

    val circuit: Parser[( Map[Gate, Boolean], Vector[Connection] )] =
      (
        input.repSep( endl ) <* endl.rep( 2 ),
        connection.repSep( endl ) <* endl.rep0
      ).mapN( ( inputs, connections ) => ( inputs.toList.toMap, connections.toNev.toVector ) )

    def numberGate( p: Char ): Parser[Int] =
      Parser.char( p ) *> Parser.char( '0' ).rep0 *>
        ( Parser.end.as( 0 ) |
          Numbers.nonNegativeIntString.mapFilter( _.toIntOption ) )

    val outputGate: Parser[Int] = numberGate( 'z' )
  }

  def parseCircuit( input: Input ): Either[String, Circuit] =
    parsers.circuit
      .parseAll( input.raw )
      .leftMap( err => s"Parser error $err" )
      .flatMap { case ( inputs, connections ) => Circuit.of( inputs, connections ) }

  override def run( input: Input ): Either[String, String] =
    parseCircuit( input )
      .map( _.evaluate )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseCircuit( input )
      .flatMap( computeLevelsAndValidate )
}
