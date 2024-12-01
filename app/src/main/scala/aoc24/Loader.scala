package aoc24

import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.functor._
import fs2.io.IOException
import fs2.io.readClassLoaderResource
import fs2.text
import fs2.text.utf8

object Loader {
  sealed trait Source {
    def source: String

    class LoadPartiallyApplied[F[_]] {
      def apply[G[_]]( puzzle: Puzzle[G], runBonus: Boolean )( implicit F: Sync[F] ): F[Input] =
        if (runBonus)
          loadInput[F]( s"${puzzle.n}.bonus.in", Some( s"${puzzle.n}.in" ) )
        else
          loadInput[F]( s"${puzzle.n}.in" )
    }

    def load[F[_]]: LoadPartiallyApplied[F] = new LoadPartiallyApplied[F]

    def loadInput[F[_]: Sync]( name: String, altName: Option[String] = None ): F[Input] =
      altName
        .foldLeft(
          readClassLoaderResource[F]( s"$source/$name" )
        )( ( s, alt ) => s.recoverWith { case _: IOException => readClassLoaderResource[F]( s"$source/$alt" ) } )
        .through( utf8.decode )
        .through( text.lines )
        .compile
        .toVector
        .map( Input )
  }

  val samples: Source = new Source { override val source: String = "samples" }
  val inputs: Source  = new Source { override val source: String = "inputs"  }
}
