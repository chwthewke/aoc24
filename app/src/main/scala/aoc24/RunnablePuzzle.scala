package aoc24

import cats.effect.IO
import language.implicitConversions

abstract class RunnablePuzzle {
  type Eff[_]

  val runner: Runnable[Eff]

  val puzzle: Puzzle[Eff]

  final def n: Int = puzzle.n

  final def run( input: Input ): IO[String] = runner.run( puzzle.run( input ) )

  final def runBonus( input: Input ): IO[String] = runner.run( puzzle.runBonus( input ) )
}

object RunnablePuzzle {
  implicit def toRunnablePuzzle[F[_]]( puzzle0: Puzzle[F] )( implicit runner0: Runnable[F] ): RunnablePuzzle =
    new RunnablePuzzle {
      override type Eff[x] = F[x]
      override val runner: Runnable[F] = runner0
      override val puzzle: Puzzle[F]   = puzzle0
    }
}
