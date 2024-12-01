package aoc24

import cats.Id

abstract class Puzzle[F[_]]( val n: Int ) {
  def run( input: Input ): F[String]

  def runBonus( input: Input ): F[String]
}

object Puzzle {
  type Pure = Puzzle[Id]
}
