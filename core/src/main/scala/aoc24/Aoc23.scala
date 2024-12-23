package aoc24

import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.semiauto
import cats.parse.Parser
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object Aoc23 extends Puzzle[Either[String, *]]( 23 ) {

  implicit class SortedMapOps[K, V]( private val self: SortedMap[K, V] ) extends AnyVal {
    def after( k: K ): SortedMap[K, V] =
      self.rangeFrom( k ).dropWhile( _._1 == k )
  }

  implicit class NesOps[A]( private val self: NonEmptySet[A] ) extends AnyVal {
    def from( a: A ): SortedSet[A] =
      self.toSortedSet.rangeFrom( a )
    def after( a: A ): SortedSet[A] =
      from( a ).dropWhile( _ == a )
  }

  case class Network( adjacencies: SortedMap[String, NonEmptySet[String]] ) {

    def triangles: Iterator[( String, String, String )] =
      for {
        ( node1, adj1 ) <- adjacencies.iterator
        node2           <- adj1.after( node1 ).iterator
        adj2 = adjacencies( node2 )
        node3 <- adj1.after( node2 ).intersect( adj2.after( node2 ) ).iterator
      } yield ( node1, node2, node3 )

    def connectedToAll( clique: NonEmptyList[String] ): Iterator[NonEmptyList[String]] =
      adjacencies
        .after( clique.head )
        .iterator
        .filter { case ( _, adj ) => clique.forall( adj.contains ) }
        .map { case ( node, _ ) => clique.prepend( node ) }

    def largestClique: NonEmptyList[String] = {

      @tailrec
      def go(
          rankNMinus1Cliques: Vector[NonEmptyList[String]],
          rankNCliques: Vector[NonEmptyList[String]]
      ): NonEmptyList[String] =
        if (rankNCliques.isEmpty)
          rankNMinus1Cliques.head.reverse
        else {
          val rankNPlus1Cliques: Vector[NonEmptyList[String]] = rankNCliques.iterator.flatMap( connectedToAll ).toVector
          go( rankNCliques, rankNPlus1Cliques )
        }

      go( Vector.empty, adjacencies.keys.map( NonEmptyList.one ).toVector )
    }

  }

  object Network {
    implicit val networkShow: Show[Network] = semiauto.show[Network]
  }

  object parsers {
    val node: Parser[String] =
      Parser.charWhere( c => c >= 'a' && c <= 'z' ).rep( 2, 2 ).string
    val link: Parser[( String, String )] =
      ( node <* Parser.char( '-' ).void, node ).tupled
  }

  def parseNetwork( input: Input ): Either[String, Network] =
    input.lines
      .foldMapM( line =>
        parsers.link.parseAll( line ).map {
          case ( n, m ) =>
            SortedMap(
              n -> NonEmptySet.one( m ),
              m -> NonEmptySet.one( n )
            )
        }
      )
      .map( Network( _ ) )
      .leftMap( err => s"Parser error $err" )

  override def run( input: Input ): Either[String, String] =
    parseNetwork( input )
      .map(
        _.triangles.count { case ( a, b, c ) => c.startsWith( "t" ) || b.startsWith( "t" ) || a.startsWith( "t" ) }
      )
      .map( _.toString )

  override def runBonus( input: Input ): Either[String, String] =
    parseNetwork( input )
      .map( _.largestClique )
      .map( _.mkString_( "," ) )
}
