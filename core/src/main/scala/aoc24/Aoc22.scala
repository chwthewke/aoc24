package aoc24

import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.mutable

object Aoc22 extends Puzzle[Either[String, *]]( 22 ) {

  case class SecretNumber( value: Long ) extends AnyVal { self =>
    def mix( f: Long => Long ): SecretNumber =
      SecretNumber( ( value ^ f( value ) ) & 0xFFFFFFL )
    def next: SecretNumber =
      mix( _ << 6 ).mix( _ >> 5 ).mix( _ << 11 )

    override def toString: String = value.toString

    @tailrec
    final def nthNext( n: Int ): SecretNumber =
      if (n == 0) this
      else next.nthNext( n - 1 )

    def price: Int = ( value % 10 ).toInt

    def pricesAndChanges: Iterator[PriceAndChanges] = new PriceAndChangesIterator( this ).drop( 3 )
  }

  case class PriceAndChanges(
      price: Int,
      changes: ChangeSeq
  ) {
    def advance( newPrice: Int ): PriceAndChanges =
      PriceAndChanges(
        newPrice,
        changes.push( newPrice - price )
      )
  }

  // changes are [-9, 9]. this is a single-int (20-bit) encoding of 4 changes
  case class ChangeSeq( value: Int ) extends AnyVal {
    def push( change: Int ): ChangeSeq =
      ChangeSeq( ( value << 5 | ( change + 9 ) ) & 0x0FFFFF )

    // most recent last
    def changes: ( Int, Int, Int, Int ) =
      (
        ( value >> 15 ) - 9,
        ( value >> 10 & 0x1F ) - 9,
        ( value >> 5 & 0x1F ) - 9,
        ( value & 0x1F ) - 9
      )
  }

  class PriceAndChangesIterator( private var sn: SecretNumber ) extends Iterator[PriceAndChanges] {
    private var pac: PriceAndChanges = PriceAndChanges( sn.price, ChangeSeq( 0 ) )

    override def hasNext: Boolean = true

    override def next(): PriceAndChanges = {
      sn = sn.next
      pac = pac.advance( sn.price )
      pac
    }
  }

  case class PriceAndChangesRecord( seen: mutable.BitSet, prices: mutable.Map[Int, Int] ) {
    def put( pac: PriceAndChanges, ix: Int ): Unit = {
      val witness: Int = ix << 20 | pac.changes.value
      if (seen.add( witness )) {
        prices.updateWith( pac.changes.value )( v => ( v.getOrElse( 0 ) + pac.price ).some )
        ()
      }
    }

    def best: Int = prices.valuesIterator.max

  }

  def bestPrice( init: Vector[SecretNumber] ): Int = {
    val iterators: Vector[Iterator[PriceAndChanges]] = init.map( _.pricesAndChanges )
    val record                                       = PriceAndChangesRecord( mutable.BitSet.empty, mutable.Map.empty )

    for {
      _ <- 4 to 2000
      j <- iterators.indices
    } {
      val p: PriceAndChanges = iterators( j ).next()
      record.put( p, j )
    }

    record.best
  }

  object SecretNumber {
    implicit val secretNumberShow: Show[SecretNumber]     = Show.fromToString
    implicit val secretNumberMonoid: Monoid[SecretNumber] = semiauto.monoid
  }

  private def parseNumbers( input: Input ): Either[String, Vector[SecretNumber]] =
    input.lines
      .traverse( _.toLongOption.map( SecretNumber( _ ) ) )
      .toRight( "Parse error" )

  override def run( input: Input ): Either[String, String] =
    parseNumbers( input )
      .map( _.foldMap( _.nthNext( 2000 ) ) )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseNumbers( input )
      .map( bestPrice )
      .map( _.toString )
}
